:- consult('input.pl').
:- consult('print.pl').
:- consult('boardManip.pl').
:- consult('utility.pl').
:- use_module(library(lists)).

play :-
    initializeGame,
    repeat,             % até ter sucesso (fimDeJogo)
        makeMove,
        endOfGame,
    showWinner.

repeat.
repeat :-
    repeat.

    