/* The PROLOG Boolean.pro computes the prime implicants and all the minimal
 sums for a given Boolean expression by the method of consensus. See text
 for further detail. */ 

%  Begin by reading the input E and invoke the computation.
qa :- nl, read(E), nl, atom_chars(E, Cs), set(Cs, [], Z), !,
    prm_implicants(Z, P), nl, !, minimal_sum(P), nl.

% Convert input to prolog set form

set([], A, [A]) :- !.
set([32|S], Ns, Z) :- set(S, Ns, Z), !.
set([X|[39|R]], Ns, Z) :- name(C, [X]), !, set(R, [n(C)| Ns], Z).
set([43|R], Ns,[Ns|Z]) :- set(R,[],Z).
set([X|R], Ns, Z) :- name(C, [X]),!, set(R, [C|Ns], Z).

% Get prime implicant.

prm_implicants(E, S) :- prm([], E, E, S).
prm(_,A,[],A) :- nl, write('prime_implicants ='),
format(',', A), nl, !.
prm(Ns, E,[A|R], S) :- consorb(A,E,R,Q),!, append(Ns, [A], Ns1),
                        complement(Q, Ns1, Rr), prm(Ns1, Q, Rr, S).

consorb(_,A,[],A) :-!.
consorb(P, E, [S|R], Q) :- ((consensus(P, S, C), !,
                            union(E,[C], F), absorb(F,F,D))
                        ; D=E ), consorb(P,D,R,Q).

consensus(A,B,Q) :- cons(A,B,Y), !, union(A,B,C),
                    complement(C,Y,Q),
                    not(cons(Q,Q,Z)).

n(X,Z) :- ((X=n(Y), Z=Y; Z=n(X))), !.
cons([], _, _) :- fail.
cons([A|Y], B, [Y|Q]) :- n(Y, Z), member(Z,X), !, Q=[Z].
cons([Y|A], B, Q) :- cons(A,B,Q).

absorb(A, [], A) :- !.
absorb(E, [H|R], F) :- del(H,E,K), !, absorb(K,R,F).

del(Q,E,E1) :- (del_if(Q,E),!,complement(E,[Q], E1)) ; E1=E.
del_if(Q,[]) :- fail, !.
del_if(Q,[X|R]) :- (Q\==X, subset(X,Q)); del_if(Q,R).

% End of prime implicants computations.

% Output Format

format(A, [E1|E2]) :- process(E1), !, tab(1),
                    ((E2 = []); (write(A), tab(1), format(A,E2))).

process([]):-!.
process([n(P)|Q2]):-write(P),write( '''' ),!,process(Q2).
process([Q1|Q2]):-write(Q1),!,process(Q2). 

% End of formatting
% Begin computation of minimal sums

minimal_sum(E):-e_var(E,Lv),
    set_of_complt_forms(E,Lv,U),!,
    prog(E,U,U,[]).

set_of_complt_forms([],_,[]):-!.
set_of_complt_forms([E|F],Lv,U):-
            complt_form(E,Lv,V),!,
            append([V],W,U),
            set_of_complt_forms(F,Lv,W).

e_var([],[]):-!.
e_var([E|F],Lv):-p_var(E,U),!,e_var(F,V),
                union(U,V,Lv).

p_var([],[]):-!.
p_var([n(X)|E],[X|U]):-p_var(E,U),!.
p_var([X|E],[X|U]):-p_var(E,U),!.

complt_form(E,Lv,U):-p_var(E,L),
                    complement(Lv,L,M),
                    proc([E],M,U1),
                    append([E],U1,U).

proc([],_,[]).

proc([E|F],[L|M],U):-(adjoin(E,L,W),
                        append(W,V,Z),
                        proc(F,[L],V) ),!,
                ( (M=[],U=Z); proc(Z,M,U) ).

adjoin(E,L1,Ue):-append(E,[L1],S1),
                append(E,[n(L1)],S2),
                append([S1],[S2],Ue).

prog(_,[],_,_):-!.
prog(E,R,U,Uprm):-simplify(E,U,R,S),!,
            check_if(E,S,Uprm,Uprm1),!,
            R=[R1|R2],prog(E,R2,U,Uprm1).

simplify(A,_,[],A).
simplify(E,U,[R|Q],S):- R=[G|H],
                    complement(U,[R],Rr),
                    if_suprfls(H,Rr),!,
                    complement(E,[G],E2),
                    simplify(E2,Rr,Q,S).
simplify(E,U,[R|Q],S):- simplify(E,U,Q,S),!.

check_if(E,E,[],[]):-nl, 
    write('minimal sum = prime sum'),!,fail.
check_if(E,S,U,U):-member(S,U);if_incld(S,U).
check_if(E,S,U,[S|U]):-nl,((U=[],write('min, sum = '));
                (tab(8),write(' = ')) ),format('+',S),nl,!.

if_suprfls([],_).
if_suprfls(V,[]):-fail,V\==[].
if_suprfls([V1|V2],T):-set_member(V1,T),!,
                        if_suprfls(V2,T).


set_member([],_).
set_member(V,[]):-fail.
set_member(V,[T1|T2]):-T1=[X|K],
( subset_set(V,K); set_member(V,T2) ).


subset_set([],_). 
subset_set(X,[]):-fail.
subset_set(X,[K|L]):- subset(X,K);subset_set(X,L).

% Minimal sums computation ends here

union([],X,X):-!.
union([X|R],Y,Z):-member(X,Y),!,union(R,Y,Z).
union([X|R],Y,[X|Z]):-union(R,Y,Z).

complement(A,[],A):-!.
complement([],_,[]):-!.
complement([X|A],B,C):-member(X,B),!,complement(A,B,C),!.
complement([X|A],B,[X|C]):-complement(A,B,C).

subset(A,A). 
subset([],A).
subset(B,A):-B=[B1|B2],member(B1,A),subset(B2,A).

if_incld(A,[B|C]):-subset(B,A);if_incld(A,C).

append([],X,X):-!.
append([A|B],C,[A|D]):-append(B,C,D).

member(X,[X|_]):-!.
member(X,[_|Y]):-member(X,Y).

not(P):-call(P),!,fail.
not(P).