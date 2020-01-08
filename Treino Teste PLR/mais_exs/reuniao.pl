:- use_module(library(clpfd)).
:- use_module(library(lists)).

reuniao(X1, X2, X3) :-
    domain([X1, X2, X3, X1Reuniao1, X1Reuniao2, X1Reuniao3, X2Reuniao1_2, X2Reuniao1_3, X2Reuniao2_3], 0, 200),

    X3 #= 30,
    sum([X1, X2, X3], #=, 200),

    X1Reuniao1 + X2Reuniao1_2 + X2Reuniao1_3 + X3 #= 130,
    X1Reuniao2 + X2Reuniao1_2 + X2Reuniao2_3 + X3 #= 135,
    X1Reuniao3 + X2Reuniao1_3 + X2Reuniao2_3 + X3 #= 65,

    X1Reuniao1 + X1Reuniao2 + X1Reuniao3 #= X1,
    X2Reuniao1_2 + X2Reuniao1_3 + X2Reuniao2_3 #= X2,

    labeling([minimize(X1)], [X1, X2, X3, X1Reuniao1, X1Reuniao2, X1Reuniao3, X2Reuniao1_2, X2Reuniao1_3, X2Reuniao2_3]).
    