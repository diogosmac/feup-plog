
getMicrobeType('A', Microbe) :-
    Microbe = a.

getMicrobeType('B', Microbe) :-
    Microbe = b.

checkValidMove(OldLine, OldColumn, NewLine, NewColumn) :-
    % TO DO: fazer este predicado