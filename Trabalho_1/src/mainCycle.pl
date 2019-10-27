:- consult('input.pl').
:- consult('print.pl').
:- consult('boardManip.pl').
:- consult('utility.pl').

% predicado que leva ao menu principal, e pergunta ao utilizador todas as informacoes que necessita para
% comecar o jogo, como por exemplo o nivel de dificuldade, etc.
% tambem inicializa o estado de jogo, fazendo "reset" ao tabuleiro e a pontuacao

% startGame :-


% ---------------------------------------------------------------------

% predicado que vai buscar o estado de jogo (tabuleiro) atual, e de quem e o turno atual

% getState(Board) :-
%       removeBoard(Board),


% ---------------------------------------------------------------------

% predicado que modifica o estado de jogo (tabuleiro e pontuacoes), simbolizando
% um turno para um jogador

% changeState :-


% ---------------------------------------------------------------------

% predicado que guarda o estado de jogo, para o proximo ciclo, e calcula o proximo jogador a jogar

% saveState(Board) :-
%       saveBoard(Board),