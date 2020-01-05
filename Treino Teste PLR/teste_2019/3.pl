:- use_module(library(clpfd)).
:- use_module(library(lists)).

pres(N, K, Vars) :-
    length(Vars, N),
    domain(Vars, 1, K),

    indices(1, Vars),

    labeling([], Vars).

indices(I, [V | Vs]) :-
    V mod 2 #\= I mod 2,
    I1 is I + 1,
    indices(I1, Vs).

indices(_, []).