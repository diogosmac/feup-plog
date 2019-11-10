:- consult('mainCycle.pl').

% Main cycle

play :-
    startGame,
    repeat,  
        once(getState(Board)),
        once(changeState(Board, NewBoard)),
        once(saveState(NewBoard)),
    game_over(NewBoard, Winner),
    showWinnerAndReset(Winner).