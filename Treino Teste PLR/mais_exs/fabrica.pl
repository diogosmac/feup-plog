:- use_module(library(clpfd)).
:- use_module(library(lists)).

fabricas(3).
produtos(6).
fabrica(1, [2-(20, 100), 3-20, 4-(20,30), 5-50]).
fabrica(2, [1-(0, 100), 3-(0, 20), 4-(10,20), 5-(20,50), 6-(10, 50)]).
fabrica(3,[1-(0,100), 2-(50,100), 3-(0,20), 4-(10,20), 5-(20,50), 6-(10,50)]).

produtos(1,[0,5,3],10,(20,150)).
produtos(2,[7,4,5],13,(10,100)).
produtos(3,[2,2,1],5,(60,250)).
produtos(4,[5,0,3],8,(30,80)).
produtos(5,[1,3,2],5,(80,350)).
produtos(6,[10,12,0],20,(10,80)).


fabricas :-
    le_inputs(ListaQuantFabricas, ListaIntQuantFabricas, ListaPrecosProdsFabrs, ListaPrecoVendaProds, ListaIntProdProds),
    