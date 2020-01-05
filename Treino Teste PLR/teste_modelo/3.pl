:- use_module(library(clpfd)).
:- use_module(library(lists)).

p3(L1, L2) :-
    length(L1, N),
    length(L2, N),
    list_to_fdset(L1, FD_L1),
    put_domain(FD_L1, L2),

    all_distinct(L2),
    put_constraints(L2),

    labeling([], L2).


put_domain(_, []).
put_domain(FD_L1, [X | L2]) :-
    X in_set FD_L1,
    put_domain(FD_L1, L2).

put_constraints([_, _]).
put_constraints([A, B, C | L2]) :-
    (A #< B #/\ B #< C) #\/ (A #> B #/\ B #> C),
    put_constraints([B, C | L2]).