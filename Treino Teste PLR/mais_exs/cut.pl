:- use_module(library(clpfd)).
:- use_module(library(lists)).

cut(Shelves, Boards, SelectedBoards) :-
    length(Shelves, NumShelves),
    length(Boards, NumBoards),
    length(SelectedBoards, NumShelves),
    domain(SelectedBoards, 1, NumBoards),

    generate_tasks(Shelves, SelectedBoards, Tasks),
    generate_machines(Boards, 1, Machines),

    cumulatives(Tasks, Machines, [bound(upper)]),

    labeling([], SelectedBoards).


generate_machines([], _, []).
generate_machines([Comp | Boards], Counter, [machine(Counter, Comp) | Machines]) :-
    NewCounter is Counter + 1,
    generate_machines(Boards, NewCounter, Machines).

generate_tasks([], [], []).
generate_tasks([Comp | Shelves], [Board | SelectedBoards], [task(1, 1, _, Comp, Board) | Tasks]) :-
    generate_tasks(Shelves, SelectedBoards, Tasks).