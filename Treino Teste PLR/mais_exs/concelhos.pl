:- use_module(library(clpfd)).
:- use_module(library(lists)).

concelho(x, 120, 410).
concelho(y, 10, 800).
concelho(z, 543, 2387).
concelho(w, 3, 38).
concelho(k, 234, 376).

concelhos(NDias, MaxDist, ConcelhosVisitados, DistTotal, TotalEleitores) :-
    le_concelhos([], ListaConcelhos, ListaDistancias, ListaEleitores, 0, NumConcelhos),

    length(ConcelhosVisitadosHelper, NDias),
    domain(ConcelhosVisitadosHelper, 0, NumConcelhos), % lista com informacao do concelho a visitar num dia (0 se nao for para visitar nenhum)

    nao_repete_concelhos(ConcelhosVisitadosHelper),

    decide_visitas(ConcelhosVisitadosHelper, ListaDistancias, ListaEleitores, DistTotal, TotalEleitores),

    MaxDist #>= DistTotal,

    append(ConcelhosVisitadosHelper, [DistTotal, TotalEleitores], AllVars),

    labeling([maximize(TotalEleitores)], AllVars),

    get_concelhos_visitados(ConcelhosVisitadosHelper, ListaConcelhos, [], ConcelhosVisitados).


nao_repete_concelhos([_]).
nao_repete_concelhos([A | ConcelhosVisitadosHelper]) :-
    nao_repete_concelhos_aux(A, ConcelhosVisitadosHelper),
    nao_repete_concelhos(ConcelhosVisitadosHelper).


nao_repete_concelhos_aux(_, []).
nao_repete_concelhos_aux(A, [B | Resto]) :-
    A #= 0 #\/ B #= 0 #\/ A #\= B,
    nao_repete_concelhos_aux(A, Resto).


get_concelhos_visitados([], _, Final, Final).
get_concelhos_visitados([Atual | ConcelhosVisitadosHelper], ListaConcelhos, ConcelhosVisitadosAux, ConcelhosVisitados) :-
    if_then_else((Atual \= 0),
                 (nth1(Atual, ListaConcelhos, Nome), append(ConcelhosVisitadosAux, [Nome], NewConcelhosVisitadosAux)),
                 (NewConcelhosVisitadosAux = ConcelhosVisitadosAux)),

    get_concelhos_visitados(ConcelhosVisitadosHelper, ListaConcelhos, NewConcelhosVisitadosAux, ConcelhosVisitados).



decide_visitas([], _, _, 0, 0).
decide_visitas([Atual | ConcelhosVisitadosHelper], ListaDistancias, ListaEleitores, DistTotal, TotalEleitores) :-
    element(ConcelhoAVisitar, ListaDistancias, Dist),
    element(ConcelhoAVisitar, ListaEleitores, NEleits),
    Atual #= ConcelhoAVisitar #\/ Atual #= 0,
    Atual #= ConcelhoAVisitar #<=> B,
    DistTotal #= (B * Dist) + RestoDist,
    TotalEleitores #= (B * NEleits) + RestoEleitores,
    decide_visitas(ConcelhosVisitadosHelper, ListaDistancias, ListaEleitores, RestoDist, RestoEleitores).


le_concelhos(ListaConcelhosAux, ListaConcelhos, [Distancia | ListaDistancias], [NEleitores | ListaEleitores], NumAux, NumConcelhos) :-
    concelho(Nome, Distancia, NEleitores),
    \+ member(Nome, ListaConcelhosAux), !,
    append(ListaConcelhosAux, [Nome], NewListaConcelhosAux),
    NewNumAux is NumAux + 1,
    le_concelhos(NewListaConcelhosAux, ListaConcelhos, ListaDistancias, ListaEleitores, NewNumAux, NumConcelhos).

le_concelhos(Lista, Lista, [], [], Max, Max).


if_then_else(I, T, _) :- I, !, T.
if_then_else(_, _, E) :- E.