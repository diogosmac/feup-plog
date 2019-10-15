:- consult('input.pl').
:- consult('print.pl').
:- consult('boardManip.pl').
:- consult('utility.pl').
:- consult('logic.pl').
:- use_module(library(lists)).

tabuleiroTeste([[' ', a, b, ' ', a, ' ', ' '],
           [' ', ' ', b, ' ', ' ', ' ', ' '],
           [' ', ' ', b, ' ', ' ', ' ', ' '],
           [' ', a, ' ', a, a, ' ', ' '],
           [' ', ' ', b, ' ', b, ' ', ' '],
           [' ', ' ', b, ' ', ' ', ' ', ' '],
           [' ', ' ', b, ' ', a, ' ', ' ']
          ]).

% ---------------------------------------------------------------------
% funcoes de teste
:- consult('print.pl').

jogaPecaTest(Linha, Coluna, Peca):-
    tabuleiro(Tab),
    format("Antes:~n~n~n", []),
    display_game(Tab, 'A'),
    format("~n~n~n", []),
    playMicrobe(Linha, Coluna, Peca, Tab, TabOut),
    format("Depois:~n~n~n", []),
    display_game(TabOut, 'A'),
    format("~n~n~n", []).


analisaPecaTest(Linha, Coluna):-
    tabuleiro(Tab),
    display_game(Tab, 'A'),
    format("~n~n~n", []),
    returnMicrobeInPos(2, 3, Tab, Peca),
    format("A peca na posicao e ~p.", [Peca]).


testPedeLinhaEColuna :-
    askLineAndColumn(Linha, Coluna),
    format("~n~nLinha: ~p; Coluna: ~p", [Linha, Coluna]).


testDisplayTab :-
    tabuleiroTeste(Tab),
    changePointsA(6),
    changePointsB(7),
    display_game(Tab, 'A').


testAskMicrobeSelect :-
    tabuleiroTeste(Board),
    display_game(Board, 'A'),
    askMicrobeSelect('A', Board, Line, Column),
    format("~n~nLinha: ~p; Coluna: ~p", [Line, Column]).


testAskMicrobeMovement :-
    tabuleiroTeste(Board),
    display_game(Board, 'A'),
    askMicrobeMovement(Board, 4, 2, Line, Column),
    format("~n~nLinha: ~p; Coluna: ~p", [Line, Column]).

testEndGame :-
    tabuleiroTeste(Board),
    display_game(Board, 'A'),
    game_over(Board, Winner),
    format("~p ~n", [Winner]).
