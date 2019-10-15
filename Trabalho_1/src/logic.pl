:- consult('input.pl').
:- consult('boardManip.pl').

% validacao e execucao de uma jogada proposta; modifica o Board passado e retorna em NewBoard
move(OldLine, OldColumn, NewLine, NewColumn, Board, NewBoard) :-
    

% ---------------------------------------------------------------------

% predicado que permite a um jogador que faca uma jogada, ou seja, que selecione
% uma posicao que tenha um microbio seu, e uma posicao para que esse microbio se mova,
% modificando o tabuleiro (se input invalido, pede outra vez). 

askAndMakeMove(BoardIn, BoardOut, Player) :-



% ---------------------------------------------------------------------

game_over(Board, Winner) :-
