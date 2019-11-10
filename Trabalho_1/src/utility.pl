getMicrobeType('A', a).
getMicrobeType('B', b).

% ---------------------------------------------------------------------

% predicado que verifica se um movimento a ser feito por um jogador e valido ou nao.
% predicado e verdadeiro se o move e valido.
% retorna tambem um IsAdjacent, para indicar tal como o nome indica se o movimento foi adjacente ou nao.

checkValidMove(MicrobeType, OldLine, OldColumn, NewLine, NewColumn, Board, IsAdjacent) :-
    verifyMicrobePositions(MicrobeType, Board, OldLine, OldColumn, NewLine, NewColumn),
    verifyMicrobeMovement(OldLine, OldColumn, NewLine, NewColumn, IsAdjacent).

% ---------------------------------------------------------------------

% predicado que verifica que as posicoes inicial e final sao validas, ou seja,
% verifica se a posicao inicial ("antiga") contem um microbio do jogador,
% e verifica se a posicao final ("nova") esta livre.

verifyMicrobePositions(MicrobeType, Board, OldLine, OldColumn, NewLine, NewColumn) :-
    OldLine > 0, OldLine < 8, OldColumn > 0, OldColumn < 8,
    NewLine > 0, NewLine < 8, NewColumn > 0, NewColumn < 8,
    returnMicrobeInPos(OldLine, OldColumn, Board, Microbe1),
    Microbe1 = MicrobeType,
    returnMicrobeInPos(NewLine, NewColumn, Board, Microbe2),
    Microbe2 = ' '.


% ---------------------------------------------------------------------

% predicado que verifica se o movimento para o microbio selecionado e valido.

% versao para movimentos adjacentes:

verifyMicrobeMovement(OldLine, OldColumn, NewLine, NewColumn, IsAdjacent) :-
    (OldLine \= NewLine; OldColumn \= NewColumn),
    LineDif is OldLine - NewLine,
    ColDif is OldColumn - NewColumn,
    abs(LineDif) < 2, abs(ColDif) < 2,
    IsAdjacent = 'yes'.

% versao para movimentos nao adjacentes:

verifyMicrobeMovement(OldLine, OldColumn, NewLine, NewColumn, IsAdjacent) :-
    (OldLine \= NewLine; OldColumn \= NewColumn),
    LineDif is OldLine - NewLine,
    ColDif is OldColumn - NewColumn,
    (abs(LineDif) =:= 0; abs(LineDif) =:= 2),
    (abs(ColDif) =:= 0; abs(ColDif) =:= 2),
    IsAdjacent = 'no'.