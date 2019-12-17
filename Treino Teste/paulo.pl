move(Num, NewNum, 1) :- NewNum is Num + 1.
move(Num, NewNum, 2) :- NewNum is Num + 2.

casa_degraus(Degraus, N, L) :-
    findall(Solution, casa_degraus_aux(0, Degraus, Solution), L),
    length(L, N).

casa_degraus_aux(Current, Next, [N]) :-
    move(Current, Next, N).

casa_degraus_aux(Current, Goal, [N | Sol]) :-
    move(Current, Next, N),
    Next < Goal,
    casa_degraus_aux(Next, Goal, Sol).
