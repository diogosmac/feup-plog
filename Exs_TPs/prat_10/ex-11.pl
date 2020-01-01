:- use_module(library(clpfd)).
:- use_module(library(lists)).

prod_maquinas(List, Profit) :-
    length(List, NaoSei),
    domain(List, 1, 2),
    automaton(List, _, List,
             [source(q0), sink(qa), sink(qb)], % 1 = a, 2 = b
             [
             arc(q0, 1, qa, [A + 3, B + 2, L + 16]),
             arc(q0, 2, qb, [A + 2, B + 3, L + 10]),
             arc(qa, 1, qa, [A + 3, B + 2, L + 16]),
             arc(qb, 1, qa, [A + 3, B + 2, L + 16]),
             arc(qb, 2, qb, [A + 2, B + 3, L + 10]),
             arc(qa, 2, qb, [A + 2, B + 3, L + 10])
             ],
             [A, B, L], [0, 0, 0], [TimeMachineA, TimeMachineB, Profit]),

             TimeMachineA #=< 8, TimeMachineB #=< 7,

             labeling([maximize(Profit)], List).