#!/usr/bin/env swipl

:- set_prolog_flag(verbose, silent).
:- [load, tiebreaker].
:- initialization(main).

main :-
    setof(S-Rs, (standings(S, Rs), maplist(label, Rs)), L),
    maplist(writeln, L),
    halt.
main :-
    halt(1).