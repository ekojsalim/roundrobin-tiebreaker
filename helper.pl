:- module(helper, [integer_si/1, (#=)/3, (#<)/3, memo/1, flatten/2, take/3]).

:- use_module(library(clpfd)).

integer_si(I) :-
    functor(I, _, 0),
    integer(I).

zo_t(0, false).
zo_t(1, true).

#=(X, Y, T) :-
    X #= Y #<==> B,
    zo_t(B, T).

#<(X, Y, T) :-
    X #< Y #<==> B,
    zo_t(B, T).

:- dynamic(memo_/1).

memo(Goal) :-
    (memo_(Goal) ->
        true;
        (once(Goal), assertz(memo_(Goal)))
    ).

% helper
flatten(List, Flat) :-
    flatten(List, Flat, []).

flatten([], Res, Res) :- !.
flatten([Head|Tail], Res, Cont) :-
    !,
    flatten(Head, Res, Cont1),
    flatten(Tail, Cont1, Cont).
flatten(Term, [Term|Cont], Cont).

take(N, _, Xs) :- N =< 0, !, N =:= 0, Xs = [].
take(_, [], []).
take(N, [X|Xs], [X|Ys]) :- M is N-1, take(M, Xs, Ys).
