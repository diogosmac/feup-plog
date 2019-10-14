:- consult('input.pl').
:- consult('boardManip.pl').

% predicado que permite a um jogador que faca uma jogada, ou seja, que selecione
% uma posicao que tenha um microbio seu, e uma posicao para que esse microbio se mova,
% modificando o tabuleiro (se input invalido, pede outra vez). 

askAndMakeMove(BoardIn, BoardOut, Player) :-