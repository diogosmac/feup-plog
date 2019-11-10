% Board - tabuleiro (lista de listas)
% PointsA - Numero de pontos do primeiro jogador
% PointsB - Numero de pontos do segundo jogador
% Player - Letra A ou B, de modo a saber quem e o proximo jogador a jogar

display_game(Board, Player) :-
    pointsA(PointsA),
    pointsB(PointsB),
    format("----------------------------------------------~n~n", []),
    printBoard(Board),
    nl,
    format("          Player A         Player B           ~n", []),
    format("            ~p                ~p              ~n", [PointsA, PointsB]),
    nl,
    format("           It is player ~ps turn.           ~n", [Player]),
    format("----------------------------------------------~n", []).



printBoard(Board) :-
    format("       ----------------------------------------- ~n", []),
    format("      |  1  |  2  |  3  |  4  |  5  |  6  |  7  |~n", []),
    format("|-----|-----|-----|-----|-----|-----|-----|-----|~n", []),
    printBoardLine(1, Board).

printBoardLine(_, []).

printBoardLine(NumLine, [X | RestLines]) :-
    format("|  ~d  |", [NumLine]),
    format("  ~p  |  ~p  |  ~p  |  ~p  |  ~p  |  ~p  |  ~p  |~n", X),
    format("|-----|-----|-----|-----|-----|-----|-----|-----|~n", []),
    NewNumLine is NumLine + 1,
    printBoardLine(NewNumLine, RestLines).

% ---------------------------------------------------------------------

    % format("-----------------------------------------------------~n~n", []),
    % format("                7th Guest: Infection                 ~n~n", []),
    % format("-----------------------------------------------------~n~n", []),
printMenu :-
    format("   ____________         __________   ___     ___   __________   __________   __________   ~n", []),
    format("   \\_______   / TH     /  _______/  /  /    /  /  /  _______/  /  _______/  /___   ___/   ~n", []),
    format("          /  /        /  /   ___   /  /    /  /  /  /____     /  /____         /  /       ~n", []),
    format("         /  /        /  /   /  /  /  /    /  /  /  _____/     \\_____  \\       /  /        ~n", []),
    format("        /  /        /  /___/  /  /  /____/  /  /  /______   _______/  /      /  /         ~n", []),
    format("       /__/         \\________/   \\_________/   \\________/  /_________/      /__/          ~n~n", []),
    format("                                     I N F E C T I O N                                    ~n~n", []),
    format("Please choose one of the following game options:~n~n", []),
    format("1: Human / Human~n~n", []),
    format("2: Human / Computer~n~n", []),
    format("3: Computer / Computer~n~n", []).

% ---------------------------------------------------------------------

printDifficultyMenu(Player) :-
    format("-----------------------------------------------------~n~n", []),
    format("Please choose a difficulty for the computer (Player ~p):~n~n", [Player]),
    format("-----------------------------------------------------~n~n", []),
    format("1: Easy~n~n", []),
    format("2: Medium~n~n", []).

% ---------------------------------------------------------------------

showWinner(Winner) :-
    Winner \= 'draw',
    format("-----------------------------------------------------~n~n", []),
    format("     CONGRATS Player ~p! You are the winner!         ~n~n", [Winner]),
    format("-----------------------------------------------------~n~n", []).

showWinner(Winner) :-
    Winner =:= 'draw',
    format("-----------------------------------------------------~n~n", []),
    format("        The game ends in a draw!                 ~n~n", [Winner]),
    format("-----------------------------------------------------~n~n", []).


% ---------------------------------------------------------------------

printInvalidMove :-
    format("~nThe move chosen by the player is invalid. Please choose another move.~n~n", []).

% ---------------------------------------------------------------------

printNoValidMoves :-
    format("~nThere are no valid moves for this player. The turn will be passed to the other player.~n~n", []).
