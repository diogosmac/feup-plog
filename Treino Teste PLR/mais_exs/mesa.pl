:- use_module(library(clpfd)).
:- use_module(library(lists)).

% 1 - Asdrubal
% 2 - Bernardete
% 3 - Cristalinda
% 4 - Demetrio
% 5 - Eleuterio
% 6 - Felismina 

table6(L) :-
    length(L, 6),
    domain(L, 1, 6),
    
    all_distinct(L),

    constrain(L, 1),

    erase_symmetry(L),

    labeling([], L).


erase_symmetry([P1, P2, P3, P4, P5, P6]) :-
    P1 #= 1,
    P6 #> P2,
    P3 #> P5.


constrain(L, CurrentPos) :-
    CurrentPos < 7, !,
    element(CurrentPos, L, Person),
    if_then_else((CurrentPos is 6),
                 (RightPos is 1),
                 (RightPos is CurrentPos + 1)),

    if_then_else((CurrentPos is 1),
                 (LeftPos is 6),
                 (LeftPos is CurrentPos - 1)),

    element(LeftPos, L, PersonLeft),
    element(RightPos, L, PersonRight),

    Person #= 1 #=> (PersonLeft #= 2 #\/ PersonRight #= 2), % asdrubal e bernardete devem ficar juntos
    Person #= 3 #=> (PersonLeft #= 4 #\/ PersonRight #= 4), % cristalinda e demetrio devem ficar juntos
    Person #= 5 #=> (PersonLeft #\= 6 #/\ PersonRight #\= 6), % eleuterio e felismina nao devem ficar juntos
    Person #= 1 #=> (PersonLeft #\= 5 #/\ PersonRight #\= 5), % eleuterio e asdrubal nao devem ficar juntos
 
    NextPos is CurrentPos + 1,
    constrain(L, NextPos).

constrain(_, _).

if_then_else(I, T, _) :- I, !, T.
if_then_else(_, _, E) :- E.