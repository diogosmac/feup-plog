:- consult('input.pl').
:- consult('print.pl').
:- consult('boardManip.pl').
:- consult('utility.pl').
:- consult('logic.pl').
:- use_module(library(lists)).

tabuleiroTeste(
    [
        [ a ,' ',' ',' ',' ',' ',' '],
        [' ', a ,' ',' ',' ',' ',' '],
        [ a , a , a , a ,' ',' ',' '],
        [ a , b , a , b , b ,' ',' '],
        [ a , a , a , b , b ,' ',' '],
        [ b , a , a , b , b ,' ',' '],
        [ b , a , a , a , b , b , a ]
    ]
).

% ---------------------------------------------------------------------
% funcoes de teste
:- consult('print.pl').

jogaPecaTest(Linha, Coluna, Peca):-
    tabuleiroTeste(Tab),
    format("Antes:~n~n~n", []),
    display_game(Tab, 'A'),
    format("~n~n~n", []),
    playMicrobe(Linha, Coluna, Peca, Tab, TabOut),
    format("Depois:~n~n~n", []),
    display_game(TabOut, 'A'),
    format("~n~n~n", []).


analisaPecaTest(Linha, Coluna):-
    tabuleiroTeste(Tab),
    returnMicrobeInPos(Linha, Coluna, Tab, Peca),
    display_game(Tab, 'A'),
    format("~n~n~n", []),
    format("A peca na posicao e ~p.", [Peca]).


testPedeLinhaEColuna :-
    askLineAndColumn(Linha, Coluna),
    format("~n~nLinha: ~p; Coluna: ~p", [Linha, Coluna]).


testDisplayTab :-
    tabuleiroTeste(Tab),
    % changePointsA(6),
    % changePointsB(7),
    updatePointsNewBoard(Tab),
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

testUpdatePoints :-
    tabuleiroTeste(Board),
    display_game(Board, 'A'),
    format("~n~n", []),
    updatePointsNewBoard(Board),
    display_game(Board, 'A').

testMove :-
    tabuleiroTeste(Board),
    display_game(Board, 'A'),
    askPlayerMove('A', OldLine, OldColumn, NewLine, NewColumn),
    move('A', OldLine, OldColumn, NewLine, NewColumn, Board, NewBoard),
    format("~n~n", []),
    display_game(NewBoard, 'B').

testMoveValueRand :-
    tabuleiroTeste(Board),
    display_game(Board, 'A'),
    value(Board, 'A', ValueA, 1),
    value(Board, 'B', ValueB, 1),
    format("~nBoard value for ~s = ~p~nBoard value for ~s = ~p~n~n", ['A', ValueA, 'B', ValueB]).

testMoveValueSmart :-
    tabuleiroTeste(Board),
    display_game(Board, 'A'),
    value(Board, 'A', ValueA, 2),
    value(Board, 'B', ValueB, 2),
    format("~nBoard value for ~s = ~p~nBoard value for ~s = ~p~n~n", ['A', ValueA, 'B', ValueB]).
