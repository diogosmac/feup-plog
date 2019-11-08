:- consult('mainCycle.pl').

play :-
    startGame,
    repeat,  
        once(getState(Board)),
        once(changeState(Board, NewBoard)),
        once(saveState(Board)),
        game_over(Board, Winner),
    showWinnerAndReset(Winner).

  