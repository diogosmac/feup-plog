:- use_module(library(clpfd)).
:- use_module(library(lists)).

doze_carros(ListaCores) :-
    length(ListaCores, 12),
    domain(ListaCores, 1, 4), % 1 - Amarelo; 2 - Verde; 3 - Vermelho; 4 - Azul,
    
    global_cardinality(ListaCores, [1-4, 2-2, 3-3, 4-3]),
    
    element(1, ListaCores, CorPrimeiro),
    element(12, ListaCores, CorUltimo),
    CorPrimeiro #= CorUltimo,

    element(2, ListaCores, CorSegundo),
    element(11, ListaCores, CorPenultimo),
    CorSegundo #= CorPenultimo,

    element(5, ListaCores, 4),

    constrain_three(ListaCores),

    constrain_sequence(ListaCores, ListaVezes),
    count(1, ListaVezes, #=, 1),

    labeling([], ListaCores),
    print(ListaCores).


constrain_three([A, B, C | Rest]) :-
    constrain_three([B, C | Rest]),
    A #\= B, A #\= C, B #\= C.

constrain_three([_, _]).


constrain_sequence([A, B, C, D | Rest], [M | RestVezes]) :-
    A #= 1 #/\ B #= 2 #/\ C #= 3 #/\ D #= 4 #<=> M,
    constrain_sequence([B, C, D | Rest], RestVezes).

constrain_sequence([_, _, _], []).