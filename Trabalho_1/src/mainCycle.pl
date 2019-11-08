:- consult('input.pl').
:- consult('print.pl').
:- consult('boardManip.pl').
:- consult('utility.pl').
:- use_module(library(lists)).

repeat.
repeat :-
    repeat.

% ---------------------------------------------------------------------

% predicado que leva ao menu principal, e pergunta ao utilizador todas as informacoes que necessita para
% comecar o jogo, como por exemplo o nivel de dificuldade, etc.
% tambem inicializa o estado de jogo, fazendo "reset" ao tabuleiro e a pontuacao

startGame :-
    printMenu,
    askBetweenValues(Option, 1, 3),
    setPlayerTypes(Option),
    askDifficulty(Option).

% ---------------------------------------------------------------------

% predicado que vai buscar o estado de jogo (tabuleiro) atual, e de quem e o turno atual

getState(Board) :-
    removeBoard(Board).


% ---------------------------------------------------------------------

% predicado que modifica o estado de jogo (tabuleiro e pontuacoes), simbolizando
% um turno para um jogador

changeState(Board, NewBoard) :-
    turn(Player),
    display_game(Board, Player),
    playerType(Player, PlayerType),
    changeStateAux(Player, PlayerType, Board, NewBoard),
    updatePointsNewBoard(NewBoard).


% if player is human
changeStateAux(Player, 'H', Board, NewBoard) :-
    valid_moves(Player, Board, ListOfValidBoards),
    checkValidMoves(Player, ListOfValidBoards, Board, NewBoard). % need to call valid_moves to see if human has valid play that he can make; if not, skips turn

checkValidMoves(Player, [], Board, Board) :-
    printNoValidMoves.

checkValidMoves(Player, _, Board, NewBoard) :-
    askPlayerMove(Player, OldLine, OldColumn, NewLine, NewColumn),
    handleMoveRepeat(Player, OldLine, OldColumn, NewLine, NewColumn, Board, NewBoard). % to handle if human inputs invalid move

% if valid move
handleMoveRepeat(Player, OldLine, OldColumn, NewLine, NewColumn, Board, NewBoard) :-
    move(Player, OldLine, OldColumn, NewLine, NewColumn, Board, NewBoard).

% if invalid move (asks again)
handleMoveRepeat(Player, _, _, _, _, Board, NewBoard) :-
    printInvalidMove,
    askPlayerMove(Player, OldLine2, OldColumn2, NewLine2, NewColumn2),
    handleMoveRepeat(Player, OldLine2, OldColumn2, NewLine2, NewColumn2, Board, NewBoard).



% if player is computer
changeStateAux(Player, 'C', Board, NewBoard) :-
    difficulty(Player, Level),
    choose_move(Level, Player, Board, NewBoard).

% ---------------------------------------------------------------------

% predicado que guarda o estado de jogo, para o proximo ciclo, e calcula o proximo jogador a jogar

saveState(Board) :-
    saveBoard(Board),
    changeTurn.

% ---------------------------------------------------------------------

% predicado que determina se o jogo ja tem um vencedor, verificando
% as condicoes de vitoria:
% - apenas existem pecas de uma das cores
% - o tabuleiro esta completamente preenchido

game_over(Board, Winner) :-
    ((getMicrobeType('B', MicrobeB),
      boardEndCheck(MicrobeB, Board));

     (getMicrobeType('A', MicrobeA),
      boardEndCheck(MicrobeA, Board));

      boardEndCheck(' ', Board)),
     
    pointsA(A),
    pointsB(B),
    declareWinner(A, B, Winner).

% ---------------------------------------------------------------------

showWinnerAndReset(Winner) :-
    showWinner(Winner),
    resetBoard,
    resetTurn,
    resetPoints,
    retractPlayerTypes,
    retractDifficulty.
