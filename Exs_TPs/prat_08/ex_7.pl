:-use_module(library(clpfd)).

peru_assado(Valor, X, Y) :-
    domain([Valor], 1, 300),
    X in 1..9,
    Y in 0..9,
    1000 * X + 670 + Y #= Number,
    Valor * 72 #= Number,
    labeling([], [Valor]).