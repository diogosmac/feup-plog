:- consult('input.pl').
:- consult('boardManip.pl').
:- use_module(library(random)).

% validacao e execucao de uma jogada proposta; modifica o Board passado e retorna em NewBoard
% OldLine e OldColumn referem-se a posicao onde esta o microbio que se pretende mexer.
% NewLine e NewColumn referem-se a posicao para onde esse microbio deve ir.
% O Player tambem e passado, de modo a saber-se qual o tipo de microbio que o jogador deve mover.
move(Player, OldLine, OldColumn, NewLine, NewColumn, Board, NewBoard) :-
    getMicrobeType(Player, MicrobeType),
    checkValidMove(MicrobeType, OldLine, OldColumn, NewLine, NewColumn, Board, IsAdjacent),
    playMicrobe(NewLine, NewColumn, MicrobeType, Board, BoardOut),
    handleIsAdjacent(IsAdjacent, OldLine, OldColumn, BoardOut, BoardOut2),
    contamineAdjacent(Player, NewLine, NewColumn, BoardOut2, NewBoard).

handleIsAdjacent('no', OldLine, OldColumn, BoardIn, BoardOut) :-
    playMicrobe(OldLine, OldColumn, ' ', BoardIn, BoardOut).

handleIsAdjacent('yes', OldLine, OldColumn, Board, Board).

% ---------------------------------------------------------------------

% predicado que, dadas a linha e a coluna da nova peça posicionada, contamina
% as pecas adjacentes do oponente

contamineAdjacent(Player, Line, Column, Board, NewBoard) :-
    getMicrobeType(Player, MicrobeType),
    AuxLine1 is Line - 1, AuxCol1 is Column - 1, contaminePosition(MicrobeType, AuxLine1, AuxCol1, Board, BoardAux1),
    AuxLine2 is Line - 1, AuxCol2 is Column, contaminePosition(MicrobeType, AuxLine2, AuxCol2, BoardAux1, BoardAux2),
    AuxLine3 is Line - 1, AuxCol3 is Column + 1, contaminePosition(MicrobeType, AuxLine3, AuxCol3, BoardAux2, BoardAux3),
    AuxLine4 is Line, AuxCol4 is Column - 1, contaminePosition(MicrobeType, AuxLine4, AuxCol4, BoardAux3, BoardAux4),
    AuxLine5 is Line, AuxCol5 is Column + 1, contaminePosition(MicrobeType, AuxLine5, AuxCol5, BoardAux4, BoardAux5),
    AuxLine6 is Line + 1, AuxCol6 is Column - 1, contaminePosition(MicrobeType, AuxLine6, AuxCol6, BoardAux5, BoardAux6),
    AuxLine7 is Line + 1, AuxCol7 is Column, contaminePosition(MicrobeType, AuxLine7, AuxCol7, BoardAux6, BoardAux7),
    AuxLine8 is Line + 1, AuxCol8 is Column + 1, contaminePosition(MicrobeType, AuxLine8, AuxCol8, BoardAux7, NewBoard).

contaminePosition(MicrobeType, Line, Column, Board, NewBoard) :-
    Line > 0, Line < 8,
    Column > 0, Column < 8,
    returnMicrobeInPos(Line, Column, Board, Microbe),
    Microbe \= MicrobeType, Microbe \= ' ',
    playMicrobe(Line, Column, MicrobeType, Board, NewBoard).

contaminePosition(MicrobeType, Line, Column, Board, Board).

% ---------------------------------------------------------------------

% predicado que permite a um jogador que faca uma jogada, ou seja, que selecione
% uma posicao que tenha um microbio seu, e uma posicao para que esse microbio se mova,
% modificando o tabuleiro (se input invalido, pede outra vez). 

% askAndMakeMove(BoardIn, BoardOut, Player) :-


% ---------------------------------------------------------------------

% predicado que mostra o vencedor do jogo, tendo em conta os pontos de cada um

declareWinner(A, B, Winner) :-
    A > B,
    Winner = 'A'.

declareWinner(A, B, Winner) :-
    B > A,
    Winner = 'B'.

declareWinner(A, B, Winner) :-
    A =:= B,
    Winner = 'draw'.


% predicado que apenas e verdadeiro se o tabuleiro nao contem em lado nenhum a peca P.

boardEndCheck(_, []).

boardEndCheck(P, [Line | Rest]) :-
    boardLineEndCheck(P, Line),
    boardEndCheck(P, Rest).

boardLineEndCheck(_, []).

boardLineEndCheck(P, [Head | Tail]) :-
    Head \= P,
    boardLineEndCheck(P, Tail).

% ---------------------------------------------------------------------

% predicado que avalia o estado de jogo, retornando um numero value para o caracterizar.
% dois niveis de dificuldade do computador:
% 1 - value e gerado aleatoriamente, de modo a que o computador escolha uma jogada qualquer possivel
% 2 - value e a diferenca de pecas dos jogadores, de modo a que o computador escolha (gananciosamente) a jogada que o poe mais em vantagem naquele turno

value(1, Board, Player, Value) :-
    random(0, 49, Value).

value(2, Board, Player, Value) :-
    updatePoints(Board, 0, 0, PointsA, PointsB),
    valueAux(Player, Value, PointsA, PointsB).

valueAux(Player, Value, PointsA, PointsB) :-
    Player = 'A', Value is PointsA-PointsB.

valueAux(Player, Value, PointsA, PointsB) :-
    Player = 'B', Value is PointsB-PointsA.


% ---------------------------------------------------------------------

% predicado a ser utilizado pelo computador, que gera um possivel movimento e produz uma jogada
% qualquer possivel tendo em conta BoardIn, sendo que o resultado dessa jogada e guardado
% em BoardOut

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

% ---------------------------------------------------------------------

% predicado que recebe uma lista com todas as opcoes de jogada (tabuleiros possiveis) e,
% tendo em conta o nivel de dificuldade, chama o predicado value para cada um dos boards,
% sendo retornado em BestBoard o tabuleiro mais vantajoso

% se nao existir nenhuma jogada valida, o tabuleiro permanece igual, e o turno e passado a frente

chooseBestBoard(Level, ListOfValidBoards, Player, Board, BestBoard) :-
    chooseBestBoardAux(Level, ListOfValidBoards, Player, -999999, Board, BestBoard).


chooseBestBoardAux(Level, [], Player, AuxValue, AuxBoard, AuxBoard) :-
    printNoValidMoves.

chooseBestBoardAux(Level, [Board | Rest], Player, AuxValue, AuxBoard, BestBoard) :-
    value(Level, Board, Player, Value),
    Value > AuxValue -> chooseBestBoardAux(Level, Rest, Player, Value, Board, BestBoard);
    chooseBestBoardAux(Level, Rest, Player, AuxValue, AuxBoard, BestBoard).

% ---------------------------------------------------------------------

% predicado que, utilizando findall c/ findMove, gera todos os possiveis tabuleiros resultantes 
% de jogadas validas, retornando-os numa lista

valid_moves(Player, Board, ListOfValidBoards) :-
    findall(BoardOut, findMove(Player, Board, BoardOut), ListOfValidBoards).


% ---------------------------------------------------------------------

% predicado a ser utilizado pelo computador, que tendo em conta o nivel de dificuldade,
% cria todos os cenarios (jogadas) possiveis e escolhe a melhor delas (c/ value), retornando
% o novo tabuleiro

choose_move(Level, Player, Board, NewBoard) :-
    valid_moves(Player, Board, ListOfValidBoards),
    chooseBestBoard(Level, ListOfValidBoards, Player, Board, NewBoard).