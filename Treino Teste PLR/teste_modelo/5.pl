:- use_module(library(clpfd)).
:- use_module(library(lists)).

wrap(Presents, PaperRolls, SelectedPaperRolls) :-
    length(Presents, N),
    length(SelectedPaperRolls, N),
    length(PaperRolls, N2),
    domain(SelectedPaperRolls, 1, N2),

    generate_tasks(Presents, Tasks, SelectedPaperRolls),
    generate_machines(PaperRolls, Machines, 1),

    cumulatives(Tasks, Machines, [bound(upper)]),

    labeling([], SelectedPaperRolls).


generate_tasks([], [], []).
generate_tasks([X | Presents], [task(1, 1, _, X, S) | Rest], [S | SelectedPaperRolls]) :-
    generate_tasks(Presents, Rest, SelectedPaperRolls).



generate_machines([], [], _).
generate_machines([Cap | PaperRolls], [machine(Counter, Cap) | Machines], Counter) :-
    NewCounter is Counter + 1,
    generate_machines(PaperRolls, Machines, NewCounter).