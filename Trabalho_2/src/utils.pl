:- use_module(library(lists)).
:- use_module(library(clpfd)).

% --------------------------------------
% predicate that will return the time needed to get from point 1 to point 2, in hours
getTime(P1, P2, Array, Hours) :-
    member((P1-P2-Minutes), Array),
    Hours is Minutes / 60.