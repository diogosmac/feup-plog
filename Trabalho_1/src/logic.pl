:- consult('input.pl').
:- use_module(library(random)).

% <move execution>

% -- Predicate that validates and executes a proposed play
% -- Player - player who requested the move (important to know the type of piece to move)
% -- OldLine - Line of the position of the microbe that should be moved
% -- OldColumn - Column of the position of the microbe that should be moved
% -- NewLine - Line of the position to which the microbe should be moved
% -- NewColumn - Column of the position to which the microbe should be moved
% -- Board - board that represents the game state before the move
% -- NewBoard - board where the changes to the game state are represented
move(Player, OldLine, OldColumn, NewLine, NewColumn, Board, NewBoard) :-
    getMicrobeType(Player, MicrobeType),
    checkValidMove(MicrobeType, OldLine, OldColumn, NewLine, NewColumn, Board, IsAdjacent),
    playMicrobe(NewLine, NewColumn, MicrobeType, Board, BoardOut),
    handleIsAdjacent(IsAdjacent, OldLine, OldColumn, BoardOut, BoardOut2),
    contaminateAdjacent(Player, NewLine, NewColumn, BoardOut2, NewBoard).

% -- Predicate that distinguishes moves to adjacent positions, for the application of the special
% -- rules for this kind of moves

% -- -- Case where the move is executed towards a non-adjacent position
handleIsAdjacent('no', OldLine, OldColumn, BoardIn, BoardOut) :-
    playMicrobe(OldLine, OldColumn, ' ', BoardIn, BoardOut).

% -- -- Case where the move is executed towards an adjacent position
handleIsAdjacent('yes', _, _, Board, Board).

% </move execution>

% <enemy piece contamination>

% -- Predicate that, given the position of the newly set piece, contaminates the
% -- adjacent enemy pieces
contaminateAdjacent(Player, Line, Column, Board, NewBoard) :-
    getMicrobeType(Player, MicrobeType),
    AuxLine1 is Line - 1, AuxCol1 is Column - 1, contaminatePosition(MicrobeType, AuxLine1, AuxCol1, Board, BoardAux1),
    AuxLine2 is Line - 1, AuxCol2 is Column, contaminatePosition(MicrobeType, AuxLine2, AuxCol2, BoardAux1, BoardAux2),
    AuxLine3 is Line - 1, AuxCol3 is Column + 1, contaminatePosition(MicrobeType, AuxLine3, AuxCol3, BoardAux2, BoardAux3),
    AuxLine4 is Line, AuxCol4 is Column - 1, contaminatePosition(MicrobeType, AuxLine4, AuxCol4, BoardAux3, BoardAux4),
    AuxLine5 is Line, AuxCol5 is Column + 1, contaminatePosition(MicrobeType, AuxLine5, AuxCol5, BoardAux4, BoardAux5),
    AuxLine6 is Line + 1, AuxCol6 is Column - 1, contaminatePosition(MicrobeType, AuxLine6, AuxCol6, BoardAux5, BoardAux6),
    AuxLine7 is Line + 1, AuxCol7 is Column, contaminatePosition(MicrobeType, AuxLine7, AuxCol7, BoardAux6, BoardAux7),
    AuxLine8 is Line + 1, AuxCol8 is Column + 1, contaminatePosition(MicrobeType, AuxLine8, AuxCol8, BoardAux7, NewBoard).

% -- Predicate that contaminates a position of the board, if it belongs to the
% -- opposing player
% -- -- Case where the position will be contaminated
contaminatePosition(MicrobeType, Line, Column, Board, NewBoard) :-
    Line > 0, Line < 8,
    Column > 0, Column < 8,
    returnMicrobeInPos(Line, Column, Board, Microbe),
    Microbe \= MicrobeType, Microbe \= ' ',
    playMicrobe(Line, Column, MicrobeType, Board, NewBoard).

% -- -- Case where the position will not be contaminated
contaminatePosition(_, _, _, Board, Board).

% </enemy piece contamination>

% <endgame procedures>

% -- Predicate that determines the winner, depending on the scores of both players

% -- -- Case where player A has a higher score
declareWinner(A, B, Winner) :-
    A > B,
    Winner = 'A'.

% -- -- Case where player B has a higher score
declareWinner(A, B, Winner) :-
    B > A,
    Winner = 'B'.

% -- -- Case where the players have equal scores
declareWinner(A, B, Winner) :-
    A =:= B,
    Winner = 'draw'.

% -- End condition for the predicate, which iterates over the board
boardEndCheck(_, []).

% -- Predicate that iterates over the board, returning false if a piece of the
% -- type P is found
boardEndCheck(P, [Line | Rest]) :-
    boardLineEndCheck(P, Line),
    boardEndCheck(P, Rest).

% -- End condition for the predicate, which iterates over a line of the board
boardLineEndCheck(_, []).

% -- Predicate that iterates over a line of the board, returning false if a piece
% -- of the type P is found
boardLineEndCheck(P, [Head | Tail]) :-
    Head \= P,
    boardLineEndCheck(P, Tail).

% </endgame procedures>

% <move evaluation>

% predicado que avalia o estado de jogo, retornando um numero value para o caracterizar.
% dois niveis de dificuldade do computador:
% 1 - value e gerado aleatoriamente, de modo a que o computador escolha uma jogada qualquer possivel
% 2 - value e a diferenca de pecas dos jogadores, de modo a que o computador escolha (gananciosamente) a jogada que o poe mais em vantagem naquele turno

% -- Predicate that evaluates the game state, returning a value to characterize it

% -- -- Case where the difficulty is set to level 1 (easy):
% -- --     move has a random value
value(1, _, _, Value) :-
    random(0, 49, Value).

% -- -- Case where the difficulty is set to level 2 (medium):
% -- --     move's value is equal to the piece advantage of the player who executes
% -- --     the move (value can be negative!)
value(2, Board, Player, Value) :-
    updatePoints(Board, 0, 0, PointsA, PointsB),
    valueAux(Player, Value, PointsA, PointsB).

% -- Predicate that calculates the value of the play, based on the number of pieces
% -- on the board from each player

% -- -- Case where player A executes the move
valueAux(Player, Value, PointsA, PointsB) :-
    Player = 'A', Value is PointsA-PointsB.

% -- -- Case where player B executes the move
valueAux(Player, Value, PointsA, PointsB) :-
    Player = 'B', Value is PointsB-PointsA.

% </move evaluation>

% <cpu move generation>

% -- Predicate that, using findall() with the findMove() predicate, generates all possible
% -- resulting boards from the valid moves available to the player, returning them in a list
valid_moves(Player, Board, ListOfValidBoards) :-
    findall(BoardOut, findMove(Player, Board, BoardOut), ListOfValidBoards).

% -- Predicate to be used by the Computer, which generates a possible move according
% -- to the board state
% -- Player - the player for which the move is generated
% -- BoardIn - current state of the board
% -- BoardOut - state of the board after the proposed move
findMove(Player, BoardIn, BoardOut) :-
    getMicrobeType(Player, MicrobeType),
    getPositionsForMicrobe(MicrobeType, BoardIn, Line, Column),
    ((AuxLine1 is Line - 2, AuxColumn1 is Column - 2, once(move(Player, Line, Column, AuxLine1, AuxColumn1, BoardIn, BoardOut)));
     (AuxLine2 is Line - 2, AuxColumn2 is Column, once(move(Player, Line, Column, AuxLine2, AuxColumn2, BoardIn, BoardOut)));
     (AuxLine3 is Line - 2, AuxColumn3 is Column + 2, once(move(Player, Line, Column, AuxLine3, AuxColumn3, BoardIn, BoardOut)));
     (AuxLine4 is Line - 1, AuxColumn4 is Column - 1, once(move(Player, Line, Column, AuxLine4, AuxColumn4, BoardIn, BoardOut)));
     (AuxLine5 is Line - 1, AuxColumn5 is Column, once(move(Player, Line, Column, AuxLine5, AuxColumn5, BoardIn, BoardOut)));
     (AuxLine6 is Line - 1, AuxColumn6 is Column + 1, once(move(Player, Line, Column, AuxLine6, AuxColumn6, BoardIn, BoardOut)));
     (AuxLine7 is Line, AuxColumn7 is Column - 2, once(move(Player, Line, Column, AuxLine7, AuxColumn7, BoardIn, BoardOut)));
     (AuxLine8 is Line, AuxColumn8 is Column - 1, once(move(Player, Line, Column, AuxLine8, AuxColumn8, BoardIn, BoardOut)));
     (AuxLine9 is Line, AuxColumn9 is Column + 1, once(move(Player, Line, Column, AuxLine9, AuxColumn9, BoardIn, BoardOut)));
     (AuxLine10 is Line, AuxColumn10 is Column + 2, once(move(Player, Line, Column, AuxLine10, AuxColumn10, BoardIn, BoardOut)));
     (AuxLine11 is Line + 1, AuxColumn11 is Column - 1, once(move(Player, Line, Column, AuxLine11, AuxColumn11, BoardIn, BoardOut)));
     (AuxLine12 is Line + 1, AuxColumn12 is Column, once(move(Player, Line, Column, AuxLine12, AuxColumn12, BoardIn, BoardOut)));
     (AuxLine13 is Line + 1, AuxColumn13 is Column + 1, once(move(Player, Line, Column, AuxLine13, AuxColumn13, BoardIn, BoardOut)));
     (AuxLine14 is Line + 2, AuxColumn14 is Column - 2, once(move(Player, Line, Column, AuxLine14, AuxColumn14, BoardIn, BoardOut)));
     (AuxLine15 is Line + 2, AuxColumn15 is Column, once(move(Player, Line, Column, AuxLine15, AuxColumn15, BoardIn, BoardOut)));
     (AuxLine16 is Line + 2, AuxColumn16 is Column + 2, once(move(Player, Line, Column, AuxLine16, AuxColumn16, BoardIn, BoardOut)))).

% </cpu move generation>

% <cpu move selection>

% -- Predicate that receives a list with all possible moves and, according to the difficulty
% -- level, calls the value() predicate for each move, returning in BestBoard the most
% -- advantageous board
% -- If there are no available moves, the board remains the same, and the move is skipped
% -- Level - difficulty level according to which the moves will be evaluated
% -- ListOfValidBoards - list containing the resulting boards for each possible move
% -- Player - player for which the moves are being computed
% -- Board - board that represents the game state before any move is executed
% -- BestBoard - resulting board from the most advantageous move

% -- -- Case where there are no available moves
chooseBestBoard(_, [], _, _, _) :-
    printNoValidMoves.

% -- -- Case where there are available moves
chooseBestBoard(Level, ListOfValidBoards, Player, Board, BestBoard) :-
    chooseBestBoardAux(Level, ListOfValidBoards, Player, -999999, Board, BestBoard).

% -- End condition for the predicate, which iterates over the list of possible moves
chooseBestBoardAux(_, [], _, _, AuxBoard, AuxBoard).

% -- Predicate that iterates over the list of possible moves, checking if the current
% -- move is better than the previous best option, and returning the best move
% -- available in the end
% -- Level - difficulty level to be passed to the value() predicate
% -- [Board | Rest] - Head and Tail of the list of possible boards
% -- Player - player for which the moves' value will be calculated
% -- AuxValue - variable that will store the best value obtained at any point during
% --            the iteration of the list
% -- AuxBoard - variable that will store the board for which the value is highest at
% --            any point during the iteration of the list
% -- BestBoard - field that will be returned with the board for which the value is
% --             highest in the list
chooseBestBoardAux(Level, [Board | Rest], Player, AuxValue, AuxBoard, BestBoard) :-
    value(Level, Board, Player, Value),
    Value > AuxValue -> chooseBestBoardAux(Level, Rest, Player, Value, Board, BestBoard);
    chooseBestBoardAux(Level, Rest, Player, AuxValue, AuxBoard, BestBoard).

% -- Predicate to be used by the Computer, that creates all possible scenarios and chooses,
% -- according to the difficulty level, the best possible move, returning the resulting board
choose_move(Level, Player, Board, NewBoard) :-
    valid_moves(Player, Board, ListOfValidBoards),
    chooseBestBoard(Level, ListOfValidBoards, Player, Board, NewBoard).

% </cpu move selection>
