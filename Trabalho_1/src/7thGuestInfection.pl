:- consult('input.pl').
:- consult('print.pl').
:- consult('boardManip.pl').
:- consult('utility.pl').
:- use_module(library(lists)).

% ---------------------------------------------------------------------

% cell(x, y, peca).
% atualizaCelula(X, Y, NewX, NewY, Peca) :-
%     retract(cell(X,Y,Peca)).
%     assertz(cell(NewX,NewY,Peca)).
% possível mas lento (also feio, not sexy)

% ---------------------------------------------------------------------

% format aka printf
% format(".. | ~p | ~d | .... | ~n", [el1, el2, ...]).

% ---------------------------------------------------------------------

joga :-
    inicializa,
    repeat,             % até ter sucesso (fimDeJogo)
        joga,
        fimDeJogo,
    mostraVencedor.

repeat.
repeat :-
    repeat.

    