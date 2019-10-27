:- consult('input.pl').
:- consult('print.pl').
:- consult('boardManip.pl').
:- consult('utility.pl').
:- use_module(library(lists)).

play :-
    initializeGame,
    repeat,  
        once(getState),           % até ter sucesso (fimDeJogo)
        once(makeMove),
        once(endOfGame),
    showWinner.

repeat.
repeat :-
    repeat.

    