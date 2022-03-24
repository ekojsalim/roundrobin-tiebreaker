:- use_module(library(lambda)).
:- use_module(library(lists)).
:- use_module(library(clpz)).
:- use_module(library(reif)).
:- use_module(library(pairs)).
:- use_module(library(ordsets)).
:- use_module(library(si)).
:- use_module(library(format)).
:- use_module(library(debug)).
:- use_module(library(time)).

:- meta_predicate specific_value(3, ?, ?, ?).
:- dynamic(memo_/1).

memo(Goal) :-
    (memo_(Goal) ->
        true;
        (once(Goal), assertz(memo_(Goal)))
    ).

% Data
team(1, ence).
team(2, faze).
team(3, furia).
team(4, outsiders).
team(5, sprout).
team(6, vitality).

teams(L) :- findall(X, team(_, X), L).

results([[1, 2, 1, 2, 1],
    [1, 3, _, _, _],
    [1, 4, 1, 2, 0],
    [1, 5, _, _, _],
    [1, 6, 1, 2, 0],
    [2, 3, _, _, _],
    [2, 4, 4, 2, 0],
    [2, 5, 2, 2, 0],
    [2, 6, _, _, _],
    [3, 4, 3, 2, 1],
    [3, 5, 3, 2, 0],
    [3, 6, 3, 2, 1],
    [4, 5, _, _, _],
    [4, 6, _, _, _],
    [5, 6, 6, 2, 0]]).

% results([[1, 2, 1, 2, 1],
%     [1, 3, 3, 2, 1],
%     [1, 4, 1, 2, 0],
%     [1, 5, 1, 2, 1],
%     [1, 6, 1, 2, 0],
%     [2, 3, 2, 2, 1],
%     [2, 4, 4, 2, 0],
%     [2, 5, 2, 2, 0],
%     [2, 6, 2, 2, 0],
%     [3, 4, 3, 2, 1],
%     [3, 5, 3, 2, 0],
%     [3, 6, 3, 2, 1],
%     [4, 5, 4, 2, 1],
%     [4, 6, 6, 2, 0],
%     [5, 6, 6, 2, 0]]).

result(R) :- results(Rs), member(R, Rs).
instantiated_result(Rs) :- setof(R, (result(R), ground(R)), Rs).

valid_result(R) :-
    R = [T1, T2, W, S1, S2 | _],
    R ins 0..47,
    % matchup
    [T1, T2] ins 1..6,
    T1 #< T2,
    % winner
    (W #= T1 #\/ W #= T2),
    % set score
    S1 #= 2,
    S2 in 0..1.

valid_results(Rs) :-
    length(Rs, 15),
    maplist(valid_result, Rs).

% metapredicate for abstracting away tiebreaking value logic
% specific_value(ValuePredicate, TeamList, Results, Team, Value)
relevant_result(Ts, [T1, T2 | _], T) :- call((memberd_t(T1, Ts), memberd_t(T2, Ts)), T).
relevant_results(Ts, Rs, RRs) :- tfilter(relevant_result(Ts), Rs, RRs).

specific_value(VP, Ts, Rs, T, V) :-
    memo(relevant_results(Ts, Rs, RRs)),
    maplist(call(VP, T), RRs, Vs),
    sum(Vs, #=, V).

overall_value(VP, _, Rs, T, V) :- overall_value(VP, Rs, T, V).
overall_value(VP, Rs, T, V) :-
    maplist(call(VP, T), Rs, Vs),
    sum(Vs, #=,  V).

point(T, R, V) :-
    R = [_, _, W | _],
    if_(W #= T, V #= 3, V #= 0).

map_difference(T, R, V) :-
    R = [T1, T2, W, S1, S2 | _],
    if_((T #= T1 ; T #= T2),
        if_(W #= T,
            V #= S1 - S2,
            V #= S2 - S1),
        V #= 0).
    
map_win(T, R, V) :-
    R = [T1, T2, W, S1, S2 | _],
    if_((T #= T1 ; T #= T2),
        if_(W #= T,
            V #= S1,
            V #= S2),
        V #= 0).

% round_difference(T, R, V) :-
%     R = [T1, T2, W, _, _, R1, R2 | _],
%     if_((T #= T1 ; T #= T2),
%         if_(W #= T,
%             V #= R1 - R2,
%             V #= R2 - R2),
%         V #= 0).
            
% round_win(T, R, V) :-
%     R = [T1, T2, W, _, _, R1, R2 | _],
%     if_((T #= T1 ; T #= T2),
%         if_(W #= T,
%             V #= R1,
%             V #= R2),
%         V #= 0).
    

% tibreaking logic
% tiebreakers([specific_points, specific_map_difference, specific_map_wins, overall_map_difference, overall_map_wins]).
tiebreakers([specific_value(point), specific_value(map_difference), specific_value(map_win), overall_value(map_difference), overall_value(map_win)]).

team_sort(VP, Ts, Rs, Gs) :-
    maplist(call(VP, Ts, Rs), Ts, Vs),
    label(Vs),
    pairs_keys_values(TVPairs, Vs, Ts),
    keysort(TVPairs, SortedTVPairs),
    group_pairs_by_key(SortedTVPairs, GroupedTVPairs),
    pairs_values(GroupedTVPairs, Gs).

tiebreaking_team_sort([], _, _, []).
tiebreaking_team_sort(Ts, Rs, Ps, Os) :-
    team_sort(overall_value(point), Ts, Rs, Gs),
    maplist(tiebreaking_subgroup_sort(Rs, Ps), Gs, Os).

tiebreaking_subgroup_sort(_, _, [T], [T]) :- integer_si(T).
tiebreaking_subgroup_sort(Rs, [P|Ps], G, O) :-
    length(G, LG), LG > 1,
    team_sort(P, G, Rs, SG),
    length(SG, LSG),
    if_(1 #< LSG, ( % if the predicate resolves a tie (determined by length), it will split the existing groups into >1 part
            tiebreakers(TPs),
            maplist(tiebreaking_subgroup_sort(Rs, TPs), SG, O)), % reset the predicates used
        maplist(tiebreaking_subgroup_sort(Rs, Ps), SG, O) % use remaining predicates
        ).

% helper
flatten([],[]).
flatten([Head|Tail],R) :-
	flatten(Head,New_Head),
	flatten(Tail,New_Tail),
	append(New_Head,New_Tail,R).
flatten([Head|Tail1], [Head|Tail2]) :-
	Head \= [],
	Head \= [_|_],
    flatten(Tail1,Tail2).


% high level predicates
standings(S, Rs) :-
    results(Rs),
    valid_results(Rs),
    tiebreakers(Ps),
    tiebreaking_team_sort([1,2,3,4,5,6], Rs, Ps, Os),
    flatten(Os, FOs),
    reverse(FOs, S).

qualify(T, Rs) :-
    standings(S, Rs),
    team(Id, T),
    Index in 1..3,
    element(Index, S, Id).