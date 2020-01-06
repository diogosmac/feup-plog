:- use_module(library(clpfd)).
:- use_module(library(lists)).

ups_and_downs(Min, Max, N, L) :-
    length(L, N),
    domain(L, Min, Max),

    constrain_elements(L),

    labeling([], L).

constrain_elements([A, B]) :-
    A #\= B.

constrain_elements([A, B, C | L]) :-
    (B #< A #/\ B #< C) #\/ (B #> A #/\ B #> C),
    constrain_elements([B, C | L]).