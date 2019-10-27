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
    handleIsAdjacent(IsAdjacent, OldLine, OldColumn, BoardOut, NewBoard).

handleIsAdjacent('no', OldLine, OldColumn, BoardIn, BoardOut) :-
    playMicrobe(OldLine, OldColumn, ' ', BoardIn, BoardOut).

handleIsAdjacent('yes', OldLine, OldColumn, Board, Board).

% ---------------------------------------------------------------------

% predicado que, dadas a linha e a coluna da nova peça posicionada, contamina
% as pecas adjacentes do oponente

contamineAdjacent(Player, Line, Column, Board, NewBoard) :-
    getMicrobeType(Player, MicrobeType),
    contaminePosition(MicrobeType, Line - 1, Column - 1, Board, BoardAux1),
    contaminePosition(MicrobeType, Line - 1, Column, BoardAux1, BoardAux2),
    contaminePosition(MicrobeType, Line - 1, Column + 1, BoardAux2, BoardAux3),
    contaminePosition(MicrobeType, Line, Column - 1, BoardAux3, BoardAux4),
    contaminePosition(MicrobeType, Line, Column + 1, BoardAux4, BoardAux5),
    contaminePosition(MicrobeType, Line + 1, Column - 1, BoardAux5, BoardAux6),
    contaminePosition(MicrobeType, Line + 1, Column, BoardAux6, BoardAux7),
    contaminePosition(MicrobeType, Line + 1, Column + 1, BoardAux7, NewBoard).


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
    (Player = 'A', Value is PointsA-PointsB;
    Player = 'B', Value is PointsB-PointsA).


% ---------------------------------------------------------------------

% predicado a ser utilizado pelo computador, que gera um possivel movimento e produz uma jogada
% qualquer possivel tendo em conta BoardIn, sendo que o resultado dessa jogada e guardado
% em BoardOut

% findMove(Player, BoardIn, BoardOut) :-
%     getMicrobeType(Player, MicrobeType),





% ---------------------------------------------------------------------

% predicado que recebe uma lista com todas as opcoes de jogada (tabuleiros possiveis) e,
% tendo em conta o nivel de dificuldade, chama o predicado value para cada um dos boards,
% sendo retornado em BestBoard o tabuleiro mais vantajoso

% chooseBestBoard(Level, ListOfValidBoards, Player, BestBoard) :-



% ---------------------------------------------------------------------

% predicado que, utilizando findall c/ findMove, gera todos os possiveis tabuleiros resultantes 
% de jogadas validas, retornando-os numa lista

% valid_moves(Player, Board, ListOfValidBoards) :-




% ---------------------------------------------------------------------

% predicado a ser utilizado pelo computador, que tendo em conta o nivel de dificuldade,
% cria todos os cenarios (jogadas) possiveis e escolhe a melhor delas (c/ value), retornando
% o novo tabuleiro

% choose_move(Level, Player, Board, NewBoard) :-