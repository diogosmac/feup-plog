:- use_module(library(clpfd)).
:- use_module(library(lists)).

autocarro(Persons, Seats1, Seats2) :-
    length(Persons, NumPeople),
    length(Seats1, NumPeople),
    length(Seats2, NumPeople),
    domain(Seats1, 1, NumPeople),
    domain(Seats2, 1, NumPeople),

    all_distinct(Seats1), all_distinct(Seats2),

    constrain_pairs(Seats1, Seats2),

    append(Seats1, Seats2, AllSeats),

    labeling([], AllSeats).


constrain_pairs([_], _).
constrain_pairs([A, B | Seats1], Seats2) :-
    constrain_pairs_aux(A, B, Seats2),
    constrain_pairs([B | Seats1], Seats2).

constrain_pairs_aux(_, _, [_]).
constrain_pairs_aux(A, B, [X, Y | Seats2]) :-
    (A #= X) #=> (B #\= Y),
    (A #= Y) #=> (B #\= X),
    constrain_pairs_aux(A, B, [Y | Seats2]).