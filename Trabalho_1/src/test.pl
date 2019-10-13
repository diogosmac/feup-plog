:- consult('input.pl').
:- consult('print.pl').
:- consult('boardManip.pl').
:- consult('utility.pl').
:- use_module(library(lists)).

% ---------------------------------------------------------------------
% funcoes de teste
:- consult('print.pl').

jogaPecaTest(Linha, Coluna, Peca):-
    tabuleiro(Tab),
    format("Antes:~n~n~n", []),
    imprimeInfo(Tab, 2, 3, 'A'),
    format("~n~n~n", []),
    jogaPeca(Linha, Coluna, Peca, Tab, TabOut),
    format("Depois:~n~n~n", []),
    imprimeInfo(TabOut, 2, 3, 'A'),
    format("~n~n~n", []).


analisaPecaTest(Linha, Coluna):-
    tabuleiro(Tab),
    imprimeInfo(Tab, 2, 4, 'A'),
    format("~n~n~n", []),
    analisaPecaEmPosicao(2, 3, Tab, Peca),
    format("A peca na posicao e ~p.", [Peca]).


testPedeLinhaEColuna :-
    pedeLinhaEColuna(Linha, Coluna),
    format("~n~nLinha: ~p; Coluna: ~p", [Linha, Coluna]).
