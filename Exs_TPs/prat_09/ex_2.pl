:- use_module(library(clpfd)).
:- use_module(library(lists)).

musicos(Sol) :-
    Sol = [Nomes, Instrumentos, Dias],
    Nomes = [Joao, Antonio, Francisco],
    Instrumentos = [Harpa, Violino, Piano],
    Dias = [Terca, Quinta1, Quinta2],
    append(Sol, List),
    domain(List, 1, 3),
    
    all_distinct(Nomes),
    all_distinct(Instrumentos),
    all_distinct(Dias),
    Antonio #\= Piano,
    Piano #= Terca,
    Joao #\= Violino,
    Joao #= Quinta1,
    Violino #= Quinta2,
    Joao #< Antonio, Antonio #< Francisco, % avoid repetitions

    labeling([], List).