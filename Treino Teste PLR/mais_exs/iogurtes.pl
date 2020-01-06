:- use_module(library(clpfd)).
:- use_module(library(lists)).

constroi(NEmb, Orcamento, EmbPorObjeto, CustoPorObjeto, EmbUsadas, Objetos) :-
    length(Objetos, 4),
    length(EmbPorObjeto, NumObjetos),
    domain(Objetos, 1, NumObjetos),
    EmbUsadas in 0 .. NEmb,

    all_distinct(Objetos),
    escolhe_objetos(Objetos, EmbPorObjeto, CustoPorObjeto, ListaEmbalagens, ListaPrecos),

    sum(ListaPrecos, #=<, Orcamento),
    sum(ListaEmbalagens, #=, EmbUsadas),
    EmbUsadas #=< NEmb,

    labeling([maximize(EmbUsadas)], Objetos).


escolhe_objetos([], _, _, [], []).
escolhe_objetos([Objeto | Objetos], EmbPorObjeto, CustoPorObjeto, [Embs | ListaEmbalagens], [Preco | ListaPrecos]) :-
    element(Objeto, EmbPorObjeto, Embs),
    element(Objeto, CustoPorObjeto, Preco),
    escolhe_objetos(Objetos, EmbPorObjeto, CustoPorObjeto, ListaEmbalagens, ListaPrecos).
