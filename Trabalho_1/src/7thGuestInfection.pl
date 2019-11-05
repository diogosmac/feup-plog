:- consult('mainCycle.pl').
:- use_module(library(lists)).

play :-
    startGame,
    repeat,  
        once(getState(Board)),
        once(changeState(Board, NewBoard)),
        once(saveState(Board)),
        endOfGame,
    showWinner.

repeat.
repeat :-
    repeat.

  