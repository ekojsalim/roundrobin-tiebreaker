:- use_module(library(clpfd)).
:- use_module(library(pairs)).
:- use_module(library(ordsets)).

:- use_module(reif).
:- use_module(helper).

% Data
:- [data].

teams(L) :- findall(X, team(_, X), L).

result(R) :- results(Rs), member(R, Rs).
instantiated_result(Rs) :- setof(R, (result(R), ground(R)), Rs).

valid_result(R) :-
    R = [T1, T2, W, S1, S2 | _],
    % matchup
    [T1, T2] ins 1..6,
    T1 #< T2,
    % winner
    W in 1..6,
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
    
not_qualify(T, Rs) :-
    standings(S, Rs),
    team(Id, T),
    Index in 4..6,
    element(Index, S, Id).

unknown_results(Rs, URs) :-
    list_to_ord_set(Rs, ORs),
    instantiated_result(IRs),
    ord_subtract(ORs, IRs, URs).
    
qualifying_results(T, QRs) :-
    setof(Rs, (qualify(T, Rs), maplist(label, Rs)), SRs),
    maplist(unknown_results, SRs, QRs).

not_qualifying_results(T, QRs) :-
    setof(Rs, (not_qualify(T, Rs), maplist(label, Rs)), SRs),
    maplist(unknown_results, SRs, QRs).

mq(Team) :- consult('data.pl'), qualifying_results(Team, Os), open("output.txt", write, Out), write(Out, Os), close(Out).
mnq(Team) :- consult('data.pl'), not_qualifying_results(Team, Os), open("output.txt", write, Out), write(Out, Os), close(Out).