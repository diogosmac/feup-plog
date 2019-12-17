:- use_module(library(clpfd)).
:- use_module(library(lists)).

maquinista :-
    Sol = [Emp, Nomes, Pass, Loc, Sal],
    Emp = [Revisor, Foguista, Maquinista],
    Nomes = [Ferreira, Rocha, Silva],
    Pass = [Sr_Ferreira, Sr_Rocha, Sr_Silva],
    Loc = [Detroit, Chicago, A_Meio],
    Sal = [Dez_Mil, Triplo],
    append(Sol, List),
    domain(List, 1, 3),
    all_distinct(Emp),
    all_distinct(Nomes),
    all_distinct(Pass),
    all_distinct(Loc),
    all_distinct(Sal),
    
    % constraints
    Sr_Rocha #= Detroit,
    Revisor #= A_Meio,
    Sr_Ferreira #= Dez_Mil,
    Silva #\= Foguista,
    

    element(N, Pass, Chicago),
    element(N, Emp, Revisor),