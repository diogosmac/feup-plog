:- use_module(library(clpfd)).
:- use_module(library(lists)).

maquinas(A, B, Lucro) :-
    TempoAMaq1 is 3, TempoAMaq2 is 2,
    TempoBMaq1 is 2, TempoBMaq2 is 3,
    MaxTempo1 is 8, MaxTempo2 is 7,
    LucroA is 16, LucroB is 10,

    MaxA1 is 24 // TempoAMaq1,
    MaxA2 is 24 // TempoAMaq2,
    if_then_else(MaxA2 < MaxA1, MaxA is MaxA2, MaxA is MaxA1),

    MaxB1 is 24 // TempoBMaq1,
    MaxB2 is 24 // TempoBMaq2,
    if_then_else(MaxB2 < MaxB1, MaxB is MaxB2, MaxB is MaxB1),

    A in 0 .. MaxA, B in 0 .. MaxB,

    A * TempoAMaq1 + B * TempoBMaq1 #=< MaxTempo1,
    A * TempoAMaq2 + B * TempoBMaq2 #=< MaxTempo2,
    Lucro #= A * LucroA + B * LucroB,

    labeling([maximize(Lucro)], [A, B]).



if_then_else(I, T, _) :- I, !, T.
if_then_else(_, _, E) :- E.
