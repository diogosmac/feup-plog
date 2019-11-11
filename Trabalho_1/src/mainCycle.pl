:- consult('logic.pl').
:- use_module(library(lists)).

% <game initialization>

% -- Predicate that displays the main menu, and asks the player for all the necessary information
% -- to initialize the game, such as the difficulty level(s), etc
% -- After that, initializes the game state, resetting the board and player scores
startGame :-
    printMenu,
    askBetweenValues(Option, 1, 3),
    setPlayerTypes(Option),
    askDifficulty(Option).

% </game initialization>

% <game state manipulation>

% -- Predicate that obtains the current state of the game (board)
getState(Board) :-
    removeBoard(Board).

% -- Predicate that processes a player's turn, modifying the game state (board and scores)
changeState(Board, NewBoard) :-
    turn(Player),
    display_game(Board, Player),
    playerType(Player, PlayerType),
    changeStateAux(Player, PlayerType, Board, NewBoard),
    updatePointsNewBoard(NewBoard).

% -- Predicate that processes the turn, depending on whether the player is Human or the Computer

% -- -- Case where the player is Human
% -- -- valid_moves() must be called, to check whether the human has a valid move to make
changeStateAux(Player, 'H', Board, NewBoard) :-
    valid_moves(Player, Board, ListOfValidBoards),
    checkValidMoves(Player, ListOfValidBoards, Board, NewBoard).


% -- -- Case where the player is controlled by the Computer
% -- -- Obtains the difficulty level for the Computer controlled player, and chooses
% -- -- the move based on it
changeStateAux(Player, 'C', Board, NewBoard) :-
    difficulty(Player, Level),
    choose_move(Level, Player, Board, NewBoard).

% -- -- Predicate that checks whether there are moves the player can select

% -- -- -- Case where there are no moves available (skips turn)
checkValidMoves(_, [], Board, Board) :-
    printNoValidMoves.

% -- -- -- Case where there are available moves (asks the player until he selects a valid move)
checkValidMoves(Player, _, Board, NewBoard) :-
    askPlayerMove(Player, OldLine, OldColumn, NewLine, NewColumn),
    handleMoveRepeat(Player, OldLine, OldColumn, NewLine, NewColumn, Board, NewBoard).

% -- -- Predicate that checks the user's selected move

% -- -- -- Case where the move is valid
handleMoveRepeat(Player, OldLine, OldColumn, NewLine, NewColumn, Board, NewBoard) :-
    move(Player, OldLine, OldColumn, NewLine, NewColumn, Board, NewBoard).

% -- -- -- Case where the move is not valid (asks again until a valid move is selected)
handleMoveRepeat(Player, _, _, _, _, Board, NewBoard) :-
    printInvalidMove,
    askPlayerMove(Player, OldLine2, OldColumn2, NewLine2, NewColumn2),
    handleMoveRepeat(Player, OldLine2, OldColumn2, NewLine2, NewColumn2, Board, NewBoard).



% -- Predicate that saves the game state, for the next cycle, and calculates the next player to play
saveState(Board) :-
    saveBoard(Board),
    changeTurn.

% </game state manipulation>

% <endgame procedures>

% predicado que determina se o jogo ja tem um vencedor, verificando
% as condicoes de vitoria:
% - apenas existem pecas de uma das cores
% - o tabuleiro esta completamente preenchido

% -- Predicate that determines whether the game already has a winner, checking
% -- the possible end conditions:
% -- - All pieces on the board belong to the same player
% -- - The board is completely full
game_over(Board, Winner) :-

    ((getMicrobeType('B', MicrobeB),
      boardEndCheck(MicrobeB, Board));
     
     (getMicrobeType('A', MicrobeA),
      boardEndCheck(MicrobeA, Board));

      boardEndCheck(' ', Board)),
    
    pointsA(A),
    pointsB(B),
    declareWinner(A, B, Winner).

% -- Predicate that shows the winner of the game, and resets the game state so that
% -- a new game may be started
showWinnerAndReset(Winner) :-
    turn(Player),
    board(Board),
    display_game(Board, Player),
    showWinner(Winner),
    resetBoard,
    resetTurn,
    resetPoints,
    retractPlayerTypes,
    retractDifficulty.

% </endgame procedures>
