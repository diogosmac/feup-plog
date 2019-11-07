:- dynamic board/1, pointsA/1, pointsB/1, turn/1.
:- consult('utility.pl').

% tabuleiro inicial, 7x7, a ser modificado à medida que o jogo se vai desenrolando.
board([[b, ' ', ' ', ' ', ' ', ' ', a],
       [' ', ' ', ' ', ' ', ' ', ' ', ' '],
       [' ', ' ', ' ', ' ', ' ', ' ', ' '],
       [' ', ' ', ' ', ' ', ' ', ' ', ' '],
       [' ', ' ', ' ', ' ', ' ', ' ', ' '],
       [' ', ' ', ' ', ' ', ' ', ' ', ' '],
       [a, ' ', ' ', ' ', ' ', ' ', b]
      ]).

pointsA(0).
pointsB(0).

turn('A').

% ---------------------------------------------------------------------
% ---------------------------------------------------------------------

removeBoard(Board) :-
    retract(board(Board)).

saveBoard(Board) :-
    assert(board(Board)).

% funcao para ser chamada no inicio de cada jogo, para dar reset ao tabuleiro
resetBoard :-
    removeBoard(_),
    saveBoard([[b, ' ', ' ', ' ', ' ', ' ', a],
               [' ', ' ', ' ', ' ', ' ', ' ', ' '],
               [' ', ' ', ' ', ' ', ' ', ' ', ' '],
               [' ', ' ', ' ', ' ', ' ', ' ', ' '],
               [' ', ' ', ' ', ' ', ' ', ' ', ' '],
               [' ', ' ', ' ', ' ', ' ', ' ', ' '],
               [a, ' ', ' ', ' ', ' ', ' ', b]
              ]).


changePointsA(NumPoints) :-
    retract(pointsA(_)),
    assert(pointsA(NumPoints)).


changePointsB(NumPoints) :-
    retract(pointsB(_)),
    assert(pointsB(NumPoints)).

changeTurn :-
    retract(turn(Player)),
    changeTurnAux(Player, NewPlayer),
    assert(turn(NewPlayer)).

changeTurnAux('A', 'B').
changeTurnAux('B', 'A').


resetTurn :-
    retract(turn(_)),
    assert(turn('A')).


% predicado que, consoante a opcao do menu escolhida, adiciona predicados
% que indicam o tipo de jogadores que irao participar no jogo
% opcao 1 - humano vs humano
% opcao 2 - humano vs computador
% opcao 3 - computador vs computador

setPlayerTypes(1) :-
    assert(playerType('A', 'H')),
    assert(playerType('B', 'H')).

setPlayerTypes(2) :-
    assert(playerType('A', 'H')),
    assert(playerType('B', 'C')).

setPlayerTypes(3) :-
    assert(playerType('A', 'C')),
    assert(playerType('B', 'C')).


retractPlayerTypes :-
    retract(playerType('A', _)),
    retract(playerType('B', _)).


% ---------------------------------------------------------------------

% Predicado que recebe um board, e que percorre todas as posicoes, contando
% o numero de pecas de cada tipo, atualizando no final as pontuacoes dos jogadores
updatePointsNewBoard(Board) :-
    updatePoints(Board, 0, 0, PointsA, PointsB),
    changePointsA(PointsA),
    changePointsB(PointsB).

updatePoints([], CurrPointsA, CurrPointsB, CurrPointsA, CurrPointsB).

updatePoints([Line | Rest], CurrPointsA, CurrPointsB, PointsA, PointsB) :-
    updatePointsLine(Line, 0, 0, LinePointsA, LinePointsB),
    NewCurrPointsA is CurrPointsA + LinePointsA,
    NewCurrPointsB is CurrPointsB + LinePointsB,
    updatePoints(Rest, NewCurrPointsA, NewCurrPointsB, PointsA, PointsB).


updatePointsLine([], CurrLinePointsA, CurrLinePointsB, CurrLinePointsA, CurrLinePointsB).

updatePointsLine([Pos | Rest], CurrLinePointsA, CurrLinePointsB, LinePointsA, LinePointsB) :-
    Pos = a, NewCurrLinePointsA is CurrLinePointsA + 1, NewCurrLinePointsB is CurrLinePointsB,
    updatePointsLine(Rest, NewCurrLinePointsA, NewCurrLinePointsB, LinePointsA, LinePointsB).

updatePointsLine([Pos | Rest], CurrLinePointsA, CurrLinePointsB, LinePointsA, LinePointsB) :-
    Pos = b, NewCurrLinePointsB is CurrLinePointsB + 1, NewCurrLinePointsA is CurrLinePointsA,
    updatePointsLine(Rest, NewCurrLinePointsA, NewCurrLinePointsB, LinePointsA, LinePointsB).

updatePointsLine([Pos | Rest], CurrLinePointsA, CurrLinePointsB, LinePointsA, LinePointsB) :-
    Pos = ' ',
    updatePointsLine(Rest, CurrLinePointsA, CurrLinePointsB, LinePointsA, LinePointsB).
    
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

updateLine(N, Column, Microbe, [Line | More], [Line | MoreLines]) :-
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

% predicado que retorna com valores de linha e coluna que contenham o tipo de microbio passado
% como parametro

getPositionsForMicrobe(Microbe, Board, Line, Column) :-
    getLineForMicrobe(Microbe, Board, 1, Line, Column).


getLineForMicrobe(Microbe, [L | Rest], LineAux, LineAux, ColumnAux) :-
    LineAux < 8,
    getColumnForMicrobe(Microbe, L, 1, ColumnAux).

getLineForMicrobe(Microbe, [L | Rest], LineAux, Line, Column) :-
    LineAux < 8,
    NextLine is LineAux + 1,
    getLineForMicrobe(Microbe, Rest, NextLine, Line, Column).


getColumnForMicrobe(Microbe, [Pos | Rest], ColumnAux, ColumnAux) :-
    ColumnAux < 8,
    Microbe = Pos.

getColumnForMicrobe(Microbe, [Pos | Rest], ColumnAux, Column) :-
    ColumnAux < 8,
    NextColumn is ColumnAux + 1,
    getColumnForMicrobe(Microbe, Rest, NextColumn, Column).

