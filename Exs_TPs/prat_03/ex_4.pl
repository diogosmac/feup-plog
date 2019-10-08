invert(ListIn, ListOut):-
    revert(ListIn, [], ListOut).

revert([H|T], S, R) :-
    revert(T, [H|S], R).

revert([], R, R).