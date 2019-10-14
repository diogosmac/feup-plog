:- dynamic board/1, pointsA/1, pointsB/1.
:- consult('utility.pl').

% tabuleiro initial, 7x7, a ser modificado à medida que o jogo se vai desenrolando.
board([[' ', ' ', ' ', ' ', ' ', ' ', ' '],
           [' ', ' ', ' ', ' ', ' ', ' ', ' '],
           [' ', ' ', ' ', ' ', ' ', ' ', ' '],
           [' ', ' ', ' ', ' ', ' ', ' ', ' '],
           [' ', ' ', ' ', ' ', ' ', ' ', ' '],
           [' ', ' ', ' ', ' ', ' ', ' ', ' '],
           [' ', ' ', ' ', ' ', ' ', ' ', ' ']
          ]).

pointsA(0).
pointsB(0).

removeBoard(Board) :-
    retract(board(Board)).

saveBoard(Board) :-
    assert(board(Board)).

% funcao para ser chamada no inicio de cada jogo, para dar reset ao tabuleiro
resetBoard :-
    removeBoard(_),
    saveBoard([[' ', ' ', ' ', ' ', ' ', ' ', ' '],
                     [' ', ' ', ' ', ' ', ' ', ' ', ' '],
                     [' ', ' ', ' ', ' ', ' ', ' ', ' '],
                     [' ', ' ', ' ', ' ', ' ', ' ', ' '],
                     [' ', ' ', ' ', ' ', ' ', ' ', ' '],
                     [' ', ' ', ' ', ' ', ' ', ' ', ' '],
                     [' ', ' ', ' ', ' ', ' ', ' ', ' ']
                    ]).


changePointsA(NumPoints) :-
    retract(pointsA(_)),
    assert(pointsA(NumPoints)).


changePointsB(NumPoints) :-
    retract(pointsB(_)),
    assert(pointsB(NumPoints)).

% ---------------------------------------------------------------------

% introduz uma determinada peça numa determinada posiçao do tabuleiro
% Line - Linha que queremos (a comecar em 1)
% Column - Coluna que queremos (a comecar em 1)
% Microbe - Microbio (peca) a ser jogada
% BoardIn - Tabuleiro input, antes de a peça ser jogada
% BoardOut - Tabuleiro output, retornado depois de a peça ter sido colocada na posiçao

playMicrobe(Line, Column, Microbe, BoardIn, BoardOut) :-
    updateLine(Line, Column, Microbe, BoardIn, BoardOut).

updateLine(1, Column, Microbe, [Line | More], [NewLine | More]) :-
    updateColumn(Column, Microbe, Line, NewLine).

updateLine(N, Column, Microbe, [Line | More], [NewLine | MoreLines]) :-
    N > 1,
    Next is N-1,
    updateLine(Next, Column, Microbe, More, MoreLines).

updateColumn(1, Microbe, [_ | Rest], [Microbe | Rest]).
updateColumn(N, Microbe, [P | Rest], [P | More]) :-
    N > 1,
    Next is N - 1,
    updateColumn(Next, Microbe, Rest, More).

% ---------------------------------------------------------------------

% retorna atraves de Peca o que se encontra numa determinada posicao do tabuleiro
% Line - Linha que queremos (a comecar em 1)
% Column - Coluna que queremos (a comecar em 1)
% Board - Estado do tabuleiro atual
% Microbe - Peca que e retonada pelo predicado

returnMicrobeInPos(Line, Column, Board, Microbe) :-
    analizeLine(Line, Column, Board, Microbe).

analizeLine(1, Column, [Line | More], Microbe) :-
    analizeColumn(Column, Line, Microbe).

analizeLine(N, Column, [Line | More], Microbe) :-
    N > 1,
    Next is N-1,
    analizeLine(Next, Column, More, Microbe).

analizeColumn(1, [Microbe | Rest], Microbe).

analizeColumn(N, [P | Rest], Microbe) :-
    N > 1,
    Next is N - 1,
    analizeColumn(Next, Rest, Microbe).

% ---------------------------------------------------------------------