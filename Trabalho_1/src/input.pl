:- consult('boardManip.pl').
:- consult('print.pl').

readInt(Prompt, Number) :-
    write(Prompt), write('> '),
    read(Number).

% ---------------------------------------------------------------------
% Predicado que pede uma linha e coluna (entre 1 e 7) ao utilizador.
% Line e Column sao ambos retornados pelo predicado

askLineAndColumn(Line, Column) :-
    readInt('Insert the number of a line (1 to 7) ', LineIn),
    readInt('Insert the number of a column (1 to 7) ', ColumnIn),
    verifyLineAndColumn(LineIn, ColumnIn, Line, Column).

% feito de modo a que os ultimos dois argumentos sejam iguais a inputs validos;
% os primeiros dois sao os inputs que o utilizador da.

verifyLineAndColumn(LineIn, ColumnIn, LineIn, ColumnIn) :-
    integer(LineIn), integer(ColumnIn),
    LineIn > 0, LineIn < 8,
    ColumnIn > 0, ColumnIn < 8.

verifyLineAndColumn(LineIn, ColumnIn, Line, Column) :-
    format("~n~nInvalid inputs. Please, try again.~n~n", []),
    readInt('Insert the number of a line (1 to 7) ', NewLineIn),
    readInt('Insert the number of a column (1 to 7) ', NewColumnIn),
    verifyLineAndColumn(NewLineIn, NewColumnIn, Line, Column).


% predicado que pergunta ao jogador que move e que ele pretende fazer.
% OldLine e OldColumn referem-se a posicao onde esta o microbio que se pretende mexer.
% NewLine e NewColumn referem-se a posicao para onde esse microbio deve ir.
askPlayerMove(Player, OldLine, OldColumn, NewLine, NewColumn) :-
    format("Player ~p, please select a microbe to move.~n~n", [Player]),
    askLineAndColumn(OldLine, OldColumn),
    format("~n~nPlease select a new position for that microbe.~n~n", []),
    askLineAndColumn(NewLine, NewColumn),
    format("~n~n", []).

% ---------------------------------------------------------------------

askBetweenValues(Option, Value1, Value2) :-
    readInt('Option ', OptionIn),
    verifyMenuOption(OptionIn, Option, Value1, Value2).

verifyMenuOption(OptionIn, OptionIn, Value1, Value2) :-
    integer(OptionIn),
    OptionIn >= Value1, OptionIn =< Value2.

verifyMenuOption(OptionIn, Option, Value1, Value2) :-
    format("~n~nInvalid inputs. Please, try again.~n~n", []),
    readInt('Option ', NewOptionIn),
    verifyMenuOption(NewOptionIn, Option, Value1, Value2).

% ---------------------------------------------------------------------

askDifficulty(1) :-
    assertDifficulty(1, 1). % difficulty doesn't matter; still needs to be asserted so it can later be retracted with no problem

askDifficulty(2) :-
    printDifficultyMenu('B'),
    askBetweenValues(Difficulty, 1, 2),
    assertDifficulty(1, Difficulty). % difficulty for computer A doesn't matter

askDifficulty(3) :-
    printDifficultyMenu('A'),
    askBetweenValues(DifficultyA, 1, 2),
    printDifficultyMenu('B'),
    askBetweenValues(DifficultyB, 1, 2),
    assertDifficulty(DifficultyA, DifficultyB).
    