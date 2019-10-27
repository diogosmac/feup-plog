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
    format("     |  1  |  2  |  3  |  4  |  5  |  6  |  7  |~n", []),
    format("-----|-----|-----|-----|-----|-----|-----|-----|~n", []),
    printBoardLine(1, Board).

printBoardLine(_, []).

printBoardLine(NumLine, [X | RestLines]) :-
    format("  ~d  ", [NumLine]),
    format("|  ~p  |  ~p  |  ~p  |  ~p  |  ~p  |  ~p  |  ~p  |~n", X),
    format("-----| --- | --- | --- | --- | --- | --- | --- |~n", []),
    NewNumLine is NumLine + 1,
    printBoardLine(NewNumLine, RestLines).

% ---------------------------------------------------------------------

