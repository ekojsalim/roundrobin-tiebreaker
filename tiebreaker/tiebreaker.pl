% SWI-Prolog built-ins
:- use_module(library(clpfd)).
:- use_module(library(pairs)).
:- use_module(library(ordsets)).
% Local libraries
:- use_module(reif). % Provides `if_`, based on the paper 'Indexing dif/2'
:- use_module(helper). % Helper predicates for reif integration, flattening lists and memoization

% Data Provider
% provides list of teams and results
:- [data].

% Results predicate
result(R) :- results(Rs), member(R, Rs).
instantiated_result(Rs) :- setof(R, (result(R), ground(R)), Rs). % result with no unknowns

% `relevant_result` gets matching result for a list of teams `Ts`
% useful for tiebreaking using specific value (as defined by the rulebook)
relevant_result(Ts, [T1, T2 | _], T) :- call((memberd_t(T1, Ts), memberd_t(T2, Ts)), T).
relevant_results(Ts, Rs, RRs) :- tfilter(relevant_result(Ts), Rs, RRs).

% get (concrete) unknown results
unknown_results(Rs, URs) :-
    list_to_ord_set(Rs, ORs),
    memo(instantiated_result(IRs)),
    ord_subtract(ORs, IRs, URs).

% CLP(FD) constraints for a result
% We only concern ourselves with set score and not round-score
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

% Value Metapredicates
% For brevity and abstraction, we create `specific_value` and `overall_value`
% Basically they apply a (value) predicate to relevant results and sum them
specific_value(VP, Ts, Rs, T, V) :-
    memo(relevant_results(Ts, Rs, RRs)), % memoized for efficiency
    maplist(call(VP, T), RRs, Vs),
    sum(Vs, #=, V).

overall_value(VP, _, Rs, T, V) :- overall_value(VP, Rs, T, V).
overall_value(VP, Rs, T, V) :-
    maplist(call(VP, T), Rs, Vs),
    sum(Vs, #=,  V).

% Value Predicates
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
    
% Tiebreaking Logic

% List of tiebreaking predicates used
% Note that it is a list of (partially applied) predicates
% These predicates are going to be used with the metapredicate `call`
tiebreakers([specific_value(point), specific_value(map_difference), specific_value(map_win), overall_value(map_difference), overall_value(map_win)]).

% main entrypoint for tiebreaking
tiebreaking_team_sort([], _, _, []).
tiebreaking_team_sort(Ts, Rs, Ps, Os) :-
    team_sort(overall_value(point), Ts, Rs, Gs),
    maplist(tiebreaking_subgroup_sort(Rs, Ps), Gs, Os).

% recursive logic for tiebreaking
tiebreaking_subgroup_sort(_, _, [T], [T]) :- integer_si(T). % base case - standing is sorted
% tiebreaking_subgroup_sort(_, [], T, O) :- % additional base case, if we fail to tiebreak
%     length(T, LT),
%     LT #> 1,
%     permutation(T, O). % means that standing can only be solved by round score, therefore all permutation is possible
tiebreaking_subgroup_sort(Rs, [P|Ps], G, O) :- % recursive case
    length(G, LG), LG > 1, % don't catch the basecase
    team_sort(P, G, Rs, SG),
    length(SG, LSG), % length of (sorted) groups
    if_(1 #< LSG, ( % if the predicate resolves a tie (determined by length), it will split the existing groups into >1 part
            tiebreakers(TPs),
            maplist(tiebreaking_subgroup_sort(Rs, TPs), SG, O)), % tie resolved -> reset the predicates used
        maplist(tiebreaking_subgroup_sort(Rs, Ps), SG, O) % tie unresolved -> use remaining predicates
        ).

% predicate to sort team by a value predicate
team_sort(VP, Ts, Rs, Gs) :-
    maplist(call(VP, Ts, Rs), Ts, Vs), % get the (abstract) values by mapping the list
    label(Vs), % important! get concrete value for sorting 
    pairs_keys_values(TVPairs, Vs, Ts), % pair team and values
    keysort(TVPairs, SortedTVPairs), % sort them
    group_pairs_by_key(SortedTVPairs, GroupedTVPairs), % group them
    pairs_values(GroupedTVPairs, Gs). % get only the teams

% high level logic
standings(S, Rs) :-
    results(Rs),
    valid_results(Rs),
    tiebreakers(Ps),
    tiebreaking_team_sort([1,2,3,4,5,6], Rs, Ps, S).
    % flatten(Os, FOs),
    % reverse(Os, S).

may_qualify(T, NS, Rs) :-
    standings(NS, Rs),
    flatten(NS, S),
    Indexes in 4..6,
    element(Indexes, S, T).

may_not_qualify(T, NS, Rs) :-
    standings(NS, Rs),
    flatten(NS, S),
    Indexes in 1..3,
    element(Indexes, S, T).