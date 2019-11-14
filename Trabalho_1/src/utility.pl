% <microbe types>

% -- Predicate that returns the correct symbol for each player
getMicrobeType('A', a).
getMicrobeType('B', b).

% </microbe types>

% <move validity check>

% -- Predicate that checks whether a move to be executed by a player is valid
% -- or not. The predicate is true if the move if valid.
% -- Returns an IsAdjacent value, to distinguish moves to adjacent positions,
% -- which carry a fair amount of importance to the rules of the game.
checkValidMove(MicrobeType, OldLine, OldCol, NewLine, NewCol, Board, IsAdjacent) :-
    verifyMicrobePositions(MicrobeType, Board, OldLine, OldCol, NewLine, NewCol),
    verifyMicrobeMovement(OldLine, OldCol, NewLine, NewCol, IsAdjacent).

% </move validity check>

% <move positions verification>

% -- Predicate that validates the initial and final positions of a move. For
% -- this to check out, the initial position must contain a piece from the 
% -- player, and the final position must be empty.
verifyMicrobePositions(MicrobeType, Board, OldLine, OldCol, NewLine, NewCol) :-
    OldLine > 0, OldLine < 8, OldCol > 0, OldCol < 8,
    NewLine > 0, NewLine < 8, NewCol > 0, NewCol < 8,
    returnMicrobeInPos(OldLine, OldCol, Board, Microbe1),
    Microbe1 = MicrobeType,
    returnMicrobeInPos(NewLine, NewCol, Board, Microbe2),
    Microbe2 = ' '.

% </move positions verification>

% <microbe movement validation>

% -- Predicate that checks whether the move for the selected microbe is valid.

% -- -- Case where the movement is toward an adjacent position
verifyMicrobeMovement(OldLine, OldColumn, NewLine, NewColumn, IsAdjacent) :-
    (OldLine \= NewLine; OldColumn \= NewColumn),
    LineDif is OldLine - NewLine,
    ColDif is OldColumn - NewColumn,
    abs(LineDif) < 2, abs(ColDif) < 2,
    IsAdjacent = 'yes'.

% -- -- Case where the movement is toward a non-adjacent position
verifyMicrobeMovement(OldLine, OldColumn, NewLine, NewColumn, IsAdjacent) :-
    (OldLine \= NewLine; OldColumn \= NewColumn),
    LineDif is OldLine - NewLine,
    ColDif is OldColumn - NewColumn,
    (abs(LineDif) =:= 0; abs(LineDif) =:= 2),
    (abs(ColDif) =:= 0; abs(ColDif) =:= 2),
    IsAdjacent = 'no'.

% </microbe movement validation>
