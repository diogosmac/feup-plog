fatorial(0, 1).
fatorial(1, 1).

fatorial(N, V):-
    N > 1,
    N1 is N - 1,
    fatorial(N1, V1),
    V is N * V1.