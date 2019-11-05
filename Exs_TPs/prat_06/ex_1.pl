map([], _, []).

map([X | List], Predicate, [Y | FinalList]) :-
    Function =.. [Predicate, X, Y],
    Function,
    map(List, Predicate, FinalList).

f(X,Y):-Y is X*X.
duplica(X,Y) :- Y is 2*X. 