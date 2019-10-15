:- consult('input.pl').
:- consult('boardManip.pl').

% validacao e execucao de uma jogada proposta; modifica o Board passado e retorna em NewBoard
% move(OldLine, OldColumn, NewLine, NewColumn, Board, NewBoard) :-

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

