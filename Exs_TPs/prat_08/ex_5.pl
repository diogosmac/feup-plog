:-use_module(library(clpfd)).

forte(Salas) :-
    Salas = [S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12],
    domain(Salas, 0, 12), % numero de sugas numa sala
    sum([S1, S2, S3, S4], #=, 5),
    sum([S4, S5, S6, S7], #=, 5),
    sum([S7, S8, S9, S10], #=, 5),
    sum([S10, S11, S12, S1], #=, 5),
    sum(Salas, #=, 12),
    labeling([], Salas),
    write(Salas).

