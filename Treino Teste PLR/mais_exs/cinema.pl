:- use_module(library(clpfd)).
:- use_module(library(lists)).

attend(FilmList, Goings, Worth) :-
    length(FilmList, NumFilms),
    length(Goings, NumFilms),
    domain(Goings, 0, 1), % lista com variaveis binarias
    
    length(MachinesSelected, NumFilms),
    
    generate_tasks(FilmList, MachinesSelected, Tasks),
    Machines = [machine(1, 1), machine(2, NumFilms)], % maquina 1 representa ir ver o filme, maquina 2 representa nao ver o filme
    cumulatives(Tasks, Machines, [bound(upper)]),

    get_goings_and_worth(FilmList, MachinesSelected, Goings, Worth),

    labeling([maximize(Worth)], Goings).


get_goings_and_worth([], [], [], 0).
get_goings_and_worth([(_,_,FilmWorth) | FilmList], [Machine | MachinesSelected], [WillSee | Goings], Worth) :-
    Machine #= 1 #<=> B,
    WillSee #= 1 * B,
    Worth #= (FilmWorth * B) + Rest,
    get_goings_and_worth(FilmList, MachinesSelected, Goings, Rest).

generate_tasks([], [], []).
generate_tasks([(Start,Duration,_) | FilmList], [Machine | MachinesSelected], [task(Start, Duration, _, 1, Machine) | Tasks]) :-
    generate_tasks(FilmList, MachinesSelected, Tasks).