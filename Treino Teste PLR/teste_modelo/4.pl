:- use_module(library(clpfd)).
:- use_module(library(lists)).

recipes(NOvos, TempoMax, OvosPorReceita, TempoPorReceita, OvosUsados, Receitas) :-

    length(Receitas, 4),
    length(OvosPorReceita, N),
    domain(Receitas, 1, N),
    all_distinct(Receitas),

    choose_recipes(OvosPorReceita, TempoPorReceita, Receitas, ListOvos, ListTempos),
    sum(ListOvos, #=, OvosUsados),
    sum(ListTempos, #=, SumTempos),

    OvosUsados #=< NOvos,
    SumTempos #=< TempoMax,

    append(Receitas, [OvosUsados, SumTempos], AllVars),

    labeling([maximize(OvosUsados)], AllVars).

choose_recipes(OvosPorReceita, TempoPorReceita, [I1, I2, I3, I4], [O1, O2, O3, O4], [T1, T2, T3, T4]) :-
    % chooses first recipe
    element(I1, OvosPorReceita, O1),
    element(I1, TempoPorReceita, T1),

    % chooses second recipe
    element(I2, OvosPorReceita, O2),
    element(I2, TempoPorReceita, T2),

    % chooses third recipe
    element(I3, OvosPorReceita, O3),
    element(I3, TempoPorReceita, T3),

    % chooses fourth recipe
    element(I4, OvosPorReceita, O4),
    element(I4, TempoPorReceita, T4).