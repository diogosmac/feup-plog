:- use_module(library(clpfd)).
:- use_module(library(lists)).

prog2(N,M,L1,L2) :-
    length(L1,N),
    N1 is N-1, length(L2,N1),

    domain(L1,1,M),
    domain(L2,1,M),
    
    append(L1, L2, AllVars),
    all_distinct(AllVars),

    check(L1,L2),

    constrain_first_and_last(L1),

    labeling([],AllVars).

check([_],[]).
    check([A,B|R],[X|Xs]) :-
    A+B #= X,
    check([B|R],Xs).


constrain_first_and_last_aux([Last], First) :-
    First #< Last.

constrain_first_and_last_aux([_ | Rest], First) :-
    constrain_first_and_last_aux(Rest, First).

constrain_first_and_last([First | Rest]) :-
    constrain_first_and_last_aux(Rest, First).