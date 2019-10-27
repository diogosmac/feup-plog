:- consult('ex_1.pl').

% retirar nos a medida que vamos percorrendo; se no fim a lista estiver vazia, passar

pesqProfLista(NoInicial, NoFinal, ListaNos, Solucao) :-
    callProfLista([NoInicial], NoInicial, NoFinal, ListaNos, Solucao).

callProfLista(Solucao, NoFinal, NoFinal, [], Solucao).

callProfLista(Solucao, NoInicial, NoFinal, ListaNos, SolucaoFinal) :-
    NoInicial \= NoFinal,
    ligado(NoInicial, NoIntermedio),
    (\+ member(NoIntermedio, Solucao)),
    removeNo(NoIntermedio, ListaNos, NovaListaNos),
    append(Solucao, [NoIntermedio], NovaSolucao),
    callProfLista(NovaSolucao, NoIntermedio, NoFinal, NovaListaNos, SolucaoFinal).


removeNo(NoIntermedio, ListaNos, NovaListaNos) :-
    (\+ member(NoIntermedio, ListaNos)),
    NovaListaNos = ListaNos.

removeNo(NoIntermedio, ListaNos, NovaListaNos) :-
    member(NoIntermedio, ListaNos),
    delete(ListaNos, NoIntermedio, NovaListaNos).
    
