:- use_module(library(clpfd)).
:- use_module(library(lists)).

autocarro2(Persons, LugaresNumaFila, Seats1, Seats2) :-
    length(Persons, NumPeople),
    length(Seats1, NumPeople),
    length(Seats2, NumPeople),
    domain(Seats1, 1, NumPeople),
    domain(Seats2, 1, NumPeople),

    NumFilasAux is NumPeople // LugaresNumaFila,
    Resto is NumPeople mod LugaresNumaFila,
    if_then_else((Resto is 0),
                 (NumTotalFilas is NumFilasAux),
                 (NumTotalFilas is NumFilasAux + 1)),

    all_distinct(Seats1), all_distinct(Seats2),

    constrain_filas(Seats1, Seats2, LugaresNumaFila, 1, NumTotalFilas),

    append(Seats1, Seats2, AllSeats),

    labeling([], AllSeats).

    % show_seats_geral()


show_seats_geral(Ppl,N,Seats):-
	length(Row, N), append(Row, SeatsSufix, Seats),
	!,
	write_people(Ppl, Row),
	show_seats_geral(Ppl, N, SeatsSufix).
show_seats_geral(Ppl, _, Seats):-write_people(Ppl, Seats).

write_people(Ppl, List):-
	get_ppl(Ppl, List, ListPpl), write(ListPpl), nl.
	
get_ppl(_, [], []).
get_ppl(L, [X|Xs], [P|Ps]):-
	nth1(X,L,P),
	get_ppl(L,Xs,Ps).


actually_constrain_aux_aux_fodase(_, _, _, []).
actually_constrain_aux_aux_fodase(PessoaAtual, PessoaAtual2, Pessoa1, [Pessoa2 | PessoasFila2]) :-
    (PessoaAtual #= PessoaAtual2 #/\ PessoaAtual #\= Pessoa1) #=> (Pessoa1 #\= Pessoa2),
    actually_constrain_aux_aux_fodase(PessoaAtual, PessoaAtual2, Pessoa1, PessoasFila2).

actually_constrain_aux(PessoaAtual, [], _, Aux2, LenFila2) :- Aux2 > LenFila2, !.
actually_constrain_aux(PessoaAtual, [Pessoa1 | PessoasFila1], PessoasFila2, Aux2, LenFila2) :-
    element(Aux2, PessoasFila2, PessoaAtual2),
    actually_constrain_aux_aux_fodase(PessoaAtual, PessoaAtual2, Pessoa1, PessoasFila2),
    NewAux2 is Aux2 + 1,
    actually_constrain_aux(PessoaAtual, PessoasFila1, PessoasFila2, NewAux2, LenFila2).

actually_constrain(_, Aux1, LenFila1, _, _) :- Aux1 > LenFila1, !.
actually_constrain(PessoasFila1, Aux1, LenFila1, PessoasFila2, LenFila2) :-
    element(Aux1, PessoasFila1, PessoaAtual),
    actually_constrain_aux(PessoaAtual, PessoasFila1, PessoasFila2, 1, LenFila2),
    % write('2'),
    NewAux1 is Aux1 + 1,
    actually_constrain(PessoasFila1, NewAux1, LenFila1, PessoasFila2, LenFila2).

get_fila([], _, _, [], []). % quando ja n ha mais pessoas para essa fila
get_fila(RestoSeats1, LugaresNumaFila, LugaresNumaFila, [], RestoSeats1). % quando ja retiramos todas as pessoas de uma fila
get_fila([Lugar | Seats1], LugaresNumaFila, NumPessoas, [Lugar | PessoasFila], RestoSeats1) :-
    NewNumPessoas is NumPessoas + 1,
    get_fila(Seats1, LugaresNumaFila, NewNumPessoas, PessoasFila, RestoSeats1).

constrain_filas_aux(_, _, [], NumFila2, NumTotalFilas) :- % write('---'), write(NumFila2), write(' ; '), write(NumTotalFilas), nl,
    NumFila2 > NumTotalFilas, !.

constrain_filas_aux(PessoasFila1, LugaresNumaFila, Seats2, NumFila2, NumTotalFilas) :-
    get_fila(Seats2, LugaresNumaFila, 0, PessoasFila2, RestoSeats2),
    % write(Seats2), write(' - '), write(RestoSeats2), nl,
    length(PessoasFila1, LenFila1),
    length(PessoasFila2, LenFila2),
    % write(LenFila1), write(' - '), write(LenFila2), nl,
    % write(PessoasFila2), write(' - '), write(RestoSeats2), write(' ---<'),
    actually_constrain(PessoasFila1, 1, LenFila1, PessoasFila2, LenFila2), 
    NewNumFila2 is NumFila2 + 1,
    % write('>--- '), write(RestoSeats2), nl,
    constrain_filas_aux(PessoasFila1, LugaresNumaFila, RestoSeats2, NewNumFila2, NumTotalFilas).

constrain_filas([], _, _, NumFila1, NumTotalFilas) :- NumFila1 > NumTotalFilas, !.
constrain_filas(Seats1, Seats2, LugaresNumaFila, NumFila1, NumTotalFilas) :-
    get_fila(Seats1, LugaresNumaFila, 0, PessoasFila1, RestoSeats1), % retira pessoas de uma fila
    constrain_filas_aux(PessoasFila1, LugaresNumaFila, Seats2, 1, NumTotalFilas),
    NewNumFila1 is NumFila1 + 1,
    constrain_filas(RestoSeats1, Seats2, LugaresNumaFila, NewNumFila1, NumTotalFilas).


if_then_else(I, T, _) :- I, !, T.
if_then_else(_, _, E) :- E.