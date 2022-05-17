#!/usr/bin/env swipl

:- set_prolog_flag(verbose, silent).
:- [load, tiebreaker, quine_pruned_petrick].
:- initialization(main1).

flatten_key(Key-Value, Flatkey-Value):-
    flatten(Key,Flatkey).
flatten_all(Before,After):-
    flatten_all(Before,[],After).

flatten_all([], Temp, Temp).
flatten_all([Head|Before], Temp, After):-
    flatten_key(Head, Flathead),
    flatten_all(Before, [Flathead|Temp], After).


kelompokin_output(Z, T):-
    flatten_all(Z, Y),
    keysort(Y,L),% helper

    group_pairs_by_key(L, T).% kelompokin semua by key trus valuenya di gabung

    


get_all_var_member(Input, Result):-
    get_all_var_member(Input, [], Result).
get_all_var_member([], Temp, Temp).
get_all_var_member([Head|Input], Temp, Result):-
    member(M, Head), 
    var(M),!,
    get_all_var_member(Input, [Head|Temp], Result).
get_all_var_member([_|Input], Temp, Result):-
    get_all_var_member(Input, Temp, Result).

get_all_pair_two_element(InputsContainVar, Res):-
    get_all_pair_two_element(InputsContainVar, [] , Res).
get_all_pair_two_element([Head|InputsContainVar], Temp , Res):-
    get_one_pair_input(Head, ResHead),
    get_all_pair_two_element(InputsContainVar, [ResHead|Temp] , Res).
get_all_pair_two_element([], Res , Res).

get_one_pair_input([H1,H2,_,_,_], [H1,H2]).

get_pair_input([], Result, Result).
get_pair_input([Head|Input], Temp, Result):-
    get_one_pair_input(Head,Inputtemp),
    get_pair_input(Input, [Inputtemp|Temp], Result).

get_pair_input(Input, Result):-
    get_pair_input(Input, [], Result).

create_sumofproduct(Input, Value, Result):-
    get_pair_input(Input, Input1),
    create_sumofproduct(Input1, Value, [],Result).
create_sumofproduct(_, [], Temp, Result):-
    keysort(Temp,Result).
create_sumofproduct(Input, [Head|Value], Temp, Result):-
    get_one_pair_input(Head, Head2term),
    member(Head2term, Input),!,
    get_win_and_result(Head, WR),
    create_sumofproduct(Input, Value, [Head2term-WR|Temp], Result).
create_sumofproduct(Input, [Head|Value], Temp, Result):-
    get_one_pair_input(Head, Head2term),
    \+member(Head2term, Input),
    create_sumofproduct(Input, Value, Temp, Result).

get_win_and_result(List, [Win, Result]):-
    get_win(List, Win),
    get_result(List, Result).
get_win([H1,_,H1,_,_], 1).
get_win([_,H2,H2,_,_], 0).
get_result([_,_,_,0,2], 1).
get_result([_,_,_,2,0], 1).
get_result([_,_,_,1,2], 0).
get_result([_,_,_,2,1], 0).

get_power_of_2_from_val([Val1, Val2], N, Res):-
    First is 2*N+1,
    Second is 2*N,
    Res is Val1*2**First + Val2*2**Second.
get_decimal_from_key_val([_-Val|List], Temp, Dec):-
    length(List, N),
    get_power_of_2_from_val(Val, N, Res),
    Nextdec is Temp+Res,
    get_decimal_from_key_val(List, Nextdec, Dec).
get_decimal_from_key_val([], Temp, Temp).
get_decimal_from_key_val(List, Dec):-
    get_decimal_from_key_val(List, 0 , Dec).

%get_all_index_minterm from one pair FlattenRank-ALLFinalResultMatch
process_input_and_all_value_from_tiebreak(Input, AllValuetiebreakGrouped, Result):-%literally input 
    get_all_var_member(Input, AllVarMember),
    get_all_pair_two_element(AllVarMember, Hasil),
    process_input_and_all_value_from_tiebreak(AllVarMember, AllValuetiebreakGrouped, Hasil-[], Result).
process_input_and_all_value_from_tiebreak(Input, [Valtiebreak|AllValuetiebreakGrouped], Hasil-Temp, Result):-
    create_sumofproduct(Input, Valtiebreak, ResValtiebreak),
    get_decimal_from_key_val(ResValtiebreak, DecimalValTiebreak),
    member(DecimalValTiebreak, Temp),!,
    process_input_and_all_value_from_tiebreak(Input, AllValuetiebreakGrouped, Hasil-Temp, Result).
process_input_and_all_value_from_tiebreak(Input, [Valtiebreak|AllValuetiebreakGrouped], Hasil-Temp, Result):-
    create_sumofproduct(Input, Valtiebreak, ResValtiebreak),
    get_decimal_from_key_val(ResValtiebreak, DecimalValTiebreak),
    \+member(DecimalValTiebreak, Temp),
    process_input_and_all_value_from_tiebreak(Input, AllValuetiebreakGrouped, Hasil-[DecimalValTiebreak|Temp], Result).
process_input_and_all_value_from_tiebreak(_, [], Result, Result).
sorting(List, Sorted):-
    sort(List, Sorted), sort(Sorted, Sorted).

pairing_minterm_and_key_input([Hi|Input], [Hm1,Hm2|Minterm], Temp, Res):-
    pairing_minterm_and_key_input(Input, Minterm, [Hi-[Hm1,Hm2]|Temp], Res).
pairing_minterm_and_key_input([],[],Temp, Temp).
pairing_minterm_and_key_input(Input, Minterm, Res):-
    sorting(Input, SortedInput),
    pairing_minterm_and_key_input(SortedInput, Minterm, [], Res).


pairing_sop_and_key_input(Input, Sop, Res):-
    pairing_sop_and_key_input(Input, Sop, [], Res).
pairing_sop_and_key_input(Input, [Hs|Sop], Temp, Res):-
    pairing_minterm_and_key_input(Input, Hs, ResMinterm),
    pairing_sop_and_key_input(Input, Sop, [ResMinterm|Temp], Res).
pairing_sop_and_key_input(_, [], Res, Res).

list_rank(S,S).
get_list_rank([T1,T2,T3,T4,T5,T6], Listrank):-
    team(T1, Team1),
    team(T2, Team2),
    team(T3, Team3),
    team(T4, Team4),
    team(T5, Team5),
    team(T6, Team6),
    list_rank([T6-Team6, T5-Team5, T4-Team4, T3-Team3, T2-Team2, T1-Team1], Listrank).
process_result_tiebreak(InputData,Tiebreaklist, Finalres):-
    process_result_tiebreak(InputData,Tiebreaklist,[], Finalres).
process_result_tiebreak(_, [], Finalres, Finalres).
process_result_tiebreak(InputData,[FlattenRank-PredMatchResult|Tiebreaklist],Temp, Finalres):-
    process_input_and_all_value_from_tiebreak(InputData, PredMatchResult, Pair-IndexMinterm),
    length(Pair, N), M is 2*N,
    atomic_list_concat(IndexMinterm, ',', IndexMintermInString),
    quine(M, IndexMintermInString, ResultQuine),
    pairing_sop_and_key_input(Pair, ResultQuine, ResultReadyToTranslate),
    process_result_tiebreak(InputData,Tiebreaklist,[FlattenRank-ResultReadyToTranslate|Temp], Finalres).

translate_winner([TeamA,_]-1):-
    write(' dimenangkan oleh '),
    write(TeamA).
translate_winner([_,TeamB]-0):-
    write(' dimenangkan oleh '), 
    write(TeamB).
translate_winner(_-2).

translate_score(_-1):-
    write(' dengan skor 2-0').
translate_score(_-0):-
    write(' dengan skor 2-1').
translate_score(_-2).

translate_result_minterm_partial_for_rank(_-[2-2]).
translate_result_minterm_partial_for_rank([TeamA,TeamB]-[Winner,Score]):-%[4,6]-[2,0]
    \+Winner+Score is 4, !,
    write('Pertandingan team '),
    write(TeamA), write(' VS team '), write(TeamB),
    translate_winner([TeamA,TeamB]-Winner),
    translate_score([TeamA,TeamB]-Score).

translate_result_minterm([[_,_]-[2,2]|Rest], FirstOutted):-
    % team(TeamA, NamaTimA),
    % team(TeamB, NamaTimB),!,
    % translate_result_minterm_partial_for_rank([NamaTimA,NamaTimB]-[2,2]),
    !, translate_result_minterm(Rest, FirstOutted).

translate_result_minterm([[TeamA,TeamB]-[Winner,Score]|Rest], FirstOutted):-
    team(TeamA, NamaTimA),
    team(TeamB, NamaTimB),
    ((FirstOutted = true) -> (write(' dan ')); (write(''))),
    translate_result_minterm_partial_for_rank([NamaTimA,NamaTimB]-[Winner,Score]),
    % \+Winner+Score is 4,
    !, translate_result_minterm(Rest, true).
translate_result_minterm([], _).

translate_result_sop([HeadMinterm|RestMinterm], FirstOutted):-
    nl, ((FirstOutted = true) -> (write('atau')); (write(''))), nl,
    write('('), translate_result_minterm(HeadMinterm, false), write(')'),
    translate_result_sop(RestMinterm, true).
translate_result_sop([], _).

translate_rank_result(Rank-Description):-
    write('Kemungkinan peringkat yang akan terjadi (Peringkat teratas juara pertama):'),nl,
    get_list_rank(Rank, ListRankTim),
    maplist(writeln, ListRankTim),
    write('Dengan syarat kondisi hasil pertandingan: '),
    translate_result_sop(Description, false).
translate_all([Head|Rest]):-
    translate_rank_result(Head),
    nl, write('#########################'),nl,
    translate_all(Rest).
translate_all([]):- write('itulah semua kemungkinan yang mungkin terjadi').    
print_list_kode_dan_nama_tim:-
    write('1. '), team(1, NamaTim1), write(NamaTim1), nl,
    write('2. '), team(2, NamaTim2), write(NamaTim2), nl,
    write('3. '), team(3, NamaTim3), write(NamaTim3), nl,
    write('4. '), team(4, NamaTim4), write(NamaTim4), nl,
    write('5. '), team(5, NamaTim5), write(NamaTim5), nl,
    write('6. '), team(6, NamaTim6), write(NamaTim6), nl.
pilihan_kode(1):-
    write('Menampilkan hasil standing'),nl,
    setof(S-Rs, (standings(S, Rs), maplist(label, Rs)), L),
    results(Input),
    kelompokin_output(L, T),
    process_result_tiebreak(Input, T, Finalres),
    translate_all(Finalres).

pilihan_kode(2):-
    write('Anda perlu memilih tim mana sehingga kondisi dan peringkat tim tersebut LOLOS akan ditampilkan'),nl,
    print_list_kode_dan_nama_tim,
    write('Masukan kode tim: '),
    read(KodeTim),nl,
    team(KodeTim, NamaTim),
    write('Tim '), write(NamaTim), write(' berhasil terpilih.'),nl,
    write('Menampilkan hasil may_qualify dari tim '), write(NamaTim), nl,
    setof(S-Rs, (may_qualify(KodeTim, S, Rs), maplist(label, Rs)), L),
    results(Input),
    kelompokin_output(L, T),
    process_result_tiebreak(Input, T, Finalres),
    translate_all(Finalres).

pilihan_kode(3):-
    write('Anda perlu memilih tim mana sehingga kondisi dan peringkat tim tersebut TIDAK LOLOS akan ditampilkan'),nl,
    print_list_kode_dan_nama_tim,
    write('Masukan kode tim: '),
    read(KodeTim),nl,
    team(KodeTim, NamaTim),
    write('Tim '), write(NamaTim), write(' berhasil terpilih.'),nl,
    write('Menampilkan hasil may_qualify dari tim '), write(NamaTim), nl,
    setof(S-Rs, (may_not_qualify(KodeTim, S, Rs), maplist(label, Rs)), L),
    results(Input),
    kelompokin_output(L, T),
    process_result_tiebreak(Input, T, Finalres),
    translate_all(Finalres).

main1 :-
    write('Kode 1, akan menampilkan hasil standing'),nl,
    write('Kode 2, akan menampilkan hasil may_qualify'), nl,
    write('Kode 3, akan menampilkan hasil may_not_qualify'), nl,
    write('Masukkan kode angka: '),
    read(Pilihankode),
    pilihan_kode(Pilihankode),
    halt.
main1 :-
    halt(1).
