:- use_module(library(clpfd)).
:- use_module(library(lists)).

prat(Prateleiras, Objetos, Vars) :-
    length(Objetos, NumObjetos),
    length(Vars, NumObjetos),
    append(Prateleiras, ListaPrateleiras),
    length(ListaPrateleiras, NumPrateleiras),
    domain(Vars, 1, NumPrateleiras),

    get_max_peso(Objetos, 0, MaxPeso),
    length(SumPesosPrats, NumPrateleiras),
    domain(SumPesosPrats, 0, MaxPeso),

    generate_tasks(Objetos, Tasks, Vars),
    generate_machines(ListaPrateleiras, 1, Machines),
    cumulatives(Tasks, Machines, [bound(upper)]), % constrains volume

    get_soma_pesos_prat(Objetos, Vars, SumPesosPrats, 1), % gets sum of weights in all shelves

    length(Prateleiras, NumRows),
    nth1(1, Prateleiras, PratRow),
    length(PratRow, NumColumns),
    Rows is NumRows - 1,
    constrain_pesos(SumPesosPrats, Rows, 1, NumColumns), % constrains peso

    append(Vars, SumPesosPrats, AllVars),
    labeling([], AllVars).



constrain_pesos(_, _, ColumnCounter, Columns) :- ColumnCounter > Columns, !.
constrain_pesos(SumPesosPrats, NumRows, ColumnCounter, NumColumns) :-
    constrain_pesos_aux(SumPesosPrats, NumRows, NumRows, ColumnCounter, NumColumns),
    NewColumnCounter is ColumnCounter + 1,
    constrain_pesos(SumPesosPrats, NumRows, NewColumnCounter, NumColumns).

constrain_pesos_aux(_, 0, _, _, _).
constrain_pesos_aux(SumPesosPrats, RowCounter, NumRows, ColumnCounter, NumColumns) :-
    CompDeBaixo is (NumColumns * RowCounter + ColumnCounter),
    RowDeCima is RowCounter - 1,
    CompDeCima is (NumColumns * RowDeCima + ColumnCounter),

    element(CompDeBaixo, SumPesosPrats, PesoDeBaixo),
    element(CompDeCima, SumPesosPrats, PesoDeCima),
    PesoDeBaixo #>= PesoDeCima,

    NewRowCounter is RowCounter - 1,
    constrain_pesos_aux(SumPesosPrats, NewRowCounter, NumRows, ColumnCounter, NumColumns).


get_soma_pesos_prat(_, _, [], _).
get_soma_pesos_prat(Objetos, Vars, [SumPrat | SumPesosPrats], NumeroPratAtual) :-
    get_soma_pesos_prat_aux(Objetos, Vars, SumPrat, NumeroPratAtual),
    NewNumeroPrat is NumeroPratAtual + 1,
    get_soma_pesos_prat(Objetos, Vars, SumPesosPrats, NewNumeroPrat).


get_soma_pesos_prat_aux([], [], 0, _).
get_soma_pesos_prat_aux([Peso-_ | Objetos], [PrateleiraObj | Vars], SumPrat, NumeroPratAtual) :-
    PrateleiraObj #= NumeroPratAtual #<=> B,
    SumPrat #= (B * Peso) + RestoDoPeso,
    get_soma_pesos_prat_aux(Objetos, Vars, RestoDoPeso, NumeroPratAtual).


generate_machines([], _, []).
generate_machines([Cap | Rest], Counter, [machine(Counter, Cap) | Machines]) :-
    NewCounter is Counter + 1,
    generate_machines(Rest, NewCounter, Machines).


generate_tasks([], [], []).
generate_tasks([_-Volume | Objetos], [task(1, 1, _, Volume, Prat) | Tasks], [Prat | Vars]) :-
    generate_tasks(Objetos, Tasks, Vars).


get_max_peso([], MaxPeso, MaxPeso).
get_max_peso([Peso-_ | Objetos], Aux, MaxPeso) :-
    NewAux is Aux + Peso,
    get_max_peso(Objetos, NewAux, MaxPeso).