# member(X, [X | M]).

# member(X, [Y | Ys]):-
#     member(X, Ys).

# member(X, Ys):-
#     append(_, [X | _], L).


# last(L, X):-
#     append(_, [X], L).


nthElement(1, [X | _], X).

nthElement(Pos, [_ | M], X):-
    Pos > 1,
    NewPos is Pos - 1,
    nthElement(NewPos, M, X).
