:- use_module(library(lambda)).
:- use_module(library(lists)).
:- use_module(library(clpz)).
:- use_module(library(reif)).
:- use_module(library(pairs)).
:- use_module(library(tabling)).
:- use_module(library(format)).
:- use_module(library(debug)).

:- meta_predicate sort_group_by_predicate(4, ?, ?, ?).

team(1, ence).
team(2, faze).
team(3, furia).
team(4, outsiders).
team(5, sprout).
team(6, vitality).

teams(L) :- findall(X, team(_, X), L).

results([[1, 2, 1, 2, 1],
    [1, 3, 3, 2, 1],
    [1, 4, 1, 2, 0],
    [1, 5, _, _, _],
    [1, 6, 1, 2, 0],
    [2, 3, _, _, _],
    [2, 4, 4, 2, 0],
    [2, 5, 2, 2, 0],
    [2, 6, 2, 2, 0],
    [3, 4, 3, 2, 1],
    [3, 5, 3, 2, 0],
    [3, 6, 3, 2, 1],
    [4, 5, 4, 2, 1],
    [4, 6, _, _, _],
    [5, 6, 6, 2, 0]]).


valid_result(R) :-
    R = [T1, T2, W, S1, S2],
    R ins 0..47,
    % matchup
    [T1, T2] ins 1..6,
    T1 #< T2,
    % winner
    (W #= T1 #\/ W #= T2),
    % set score
    S1 #= 2,
    S2 in 0..1.
%     % round score
%     R1 #> R2,
%     R1 #>= 16 * S1, R2 #>= 16 * S2,
%     R1 #=< 16 * S1 + 15 * S2, R2 #=< 16 * S2 + 15 * S1.

valid_results(Rs) :-
    length(Rs, 15),
    maplist(valid_result, Rs).

% tiebreaking
% 1. Points amassed between the tied participants (direct match win > direct match loss)
% 2. Map difference between the tied participants (3:2 maps > 3:3 maps)
% 3. Number of map wins between the tied participants (3:3 maps > 2:2 maps)
% 4. Overall map difference
% 5. Overall number of map wins
% 6. Round score difference between the tied participants (23:21 > 23:22)
% 7. Number of round wins between the tied participants (24:22 > 23:21)
% 8. Overall round score difference (39:31 > 40:33)
% 9. Overall number of round wins (40:32 > 39:31)
% recursive sorting using multiple predicates

sort_group_by_predicate(P, Ts, Rs, Gs) :-
    % Goal =.. [P, Ts, Rs], % predicate with Ts & Rs as args too
    maplist(P, Ts, Vs),
    pairs_keys_values(TVPairs, Vs, Ts),
    % map_list_to_pairs(Goal, Ts, TVPairs),
    keysort(TVPairs, SortedTVPairs),
    group_pairs_by_key(SortedTVPairs, GroupedTVPairs),
    pairs_values(GroupedTVPairs, Gs).

% tiebreaking logic
% do initial sorting with overall_points
% do untied check, if untied then done
% else -> call subgroup_sort on the tied subgroups
% if subgroup_sort resolves a tie, reset the tiebreakers predicates (important)

tiebreakers([specific_points, specific_map_difference, specific_map_wins, overall_map_difference, overall_map_wins]).

sort_group_by_goal(P, Ts, Rs, Gs) :-
    Goal =.. [P, Ts, Rs],
    sort_group_by_predicate(Goal, Ts, Rs, Gs).

tiebreaking_sort([], _, _, []).
tiebreaking_sort(Ts, Rs, Ps, Os) :-
    sort_group_by_goal(overall_points, Ts, Rs, Gs),
    maplist(tiebreaking_sort_group(Rs, Ps), Gs, Os).

tiebreaking_sort_group(_, _, [X], [X]).
tiebreaking_sort_group(Rs, [P|Ps], G, O) :-
    length(G, LG),
    LG > 1,
    sort_group_by_goal(P, G, Rs, SG),
    length(SG, LSG),
    if_(1 #< LSG, % if the predicate resolves a tie (determined by length)
        (tiebreakers(TPs),
            maplist(tiebreaking_sort_group(Rs, TPs), SG, O) % reset the predicates used
        ),
        maplist(tiebreaking_sort_group(Rs, Ps), SG, O) % use remaining predicates
        ).

% the actual tiebreaking predicates
overall_points(_, Rs, T, V) :- overall_points(Rs, T, V).
overall_points(Rs, T, V) :- specific_points([1,2,3,4,5,6], Rs, T, V).
specific_points(_, [], _, 0).
specific_points(Ts, [R | Rs], T, V0) :-
    R = [T1, T2, W | _],
    if_((W #= T, memberd_t(T1, Ts), memberd_t(T2, Ts))
    , V0 #= V + 3
    , V0 #= V
    ),
    specific_points(Ts, Rs, T, V).

overall_map_difference(_, Rs, T, V) :- specific_map_difference([1,2,3,4,5,6], Rs, T, V).
specific_map_difference(_, [], _, 0).
specific_map_difference(Ts, [R | Rs], T, V0) :-
    R = [T1, T2, W, S1, S2 | _],
    if_(((T #= T1 ; T #= T2), memberd_t(T1, Ts), memberd_t(T2, Ts))
    , if_(W #= T
        , V #= V0 - (S1 - S2)
        , V #= V0 - (S2 - S1))
    % ,(Z #<==> W #= T,
    %   V #= V0 - (2 * Z - 1) * (S1 - S2)
    %  )
    , V0 #= V
    ),
    specific_map_difference(Ts, Rs, T, V).

overall_map_wins(_, Rs, T, V) :- specific_map_wins([1,2,3,4,5,6], Rs, T, V).
specific_map_wins(_, [], _, 0).
specific_map_wins(Ts, [R | Rs], T, V0) :-
    R = [T1, T2, W, S1, S2 | _],
    if_(((T #= T1 ; T #= T2), memberd_t(T1, Ts), memberd_t(T2, Ts))
    , if_(W #= T
        , V #= V0 - S1
        , V #= V0 - S2)
    , V0 #= V
    ),
    specific_map_wins(Ts, Rs, T, V).

flatten([],[]).
flatten([Head|Tail],R) :-
	flatten(Head,New_Head),
	flatten(Tail,New_Tail),
	append(New_Head,New_Tail,R).
flatten([Head|Tail1], [Head|Tail2]) :-
	Head \= [],
	Head \= [_|_],
    flatten(Tail1,Tail2).

standings(S) :-
    results(Rs),
    valid_results(Rs),
    tiebreakers(Ps),
    maplist(labeling([ffc, enum]), Rs),
    tiebreaking_sort([1,2,3,4,5,6], Rs, Ps, Os),
    flatten(Os, FOS),
    reverse(FOS, S),
    maplist(portray_clause, Rs).
    % maplist(\X^Y^(team(X, Y)), SI, S).

qualify(T) :-
    team(Id, T),
    results(Rs),
    valid_results(Rs),
    tiebreakers(Ps),
    maplist(label, Rs),
    tiebreaking_sort([1,2,3,4,5,6], Rs, Ps, Os),
    flatten(Os, FOS),
    reverse(FOS, S),
    Index in 1..3,
    element(Index, S, Id),
    portray_clause(S),
    maplist(portray_clause, Rs).

qualify(T, Ms) :-
    team(Id, T),
    results(Rs),
    valid_results(Rs),
    tiebreakers(Ps),
    maplist(label, Rs),
    tiebreaking_sort([1,2,3,4,5,6], Rs, Ps, Os),
    flatten(Os, FOS),
    reverse(FOS, S),
    Index in 1..3,
    element(Index, S, Id),
    Ms = Rs.

temp(Ms) :-
    results(Rs),
    valid_results(Rs),
    tiebreakers(Ps),
    maplist(label, Rs),
    tiebreaking_sort([1,2,3,4,5,6], Rs, Ps, Ms).