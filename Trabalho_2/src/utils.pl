:- use_module(library(lists)).
:- use_module(library(clpfd)).

% --------------------------------------
% predicate that will return the time needed to get from point 1 to point 2, in hours
getTime(P1, P2, Array, Hours) :-
    member((P1-P2-Minutes), Array),
    Hours is Minutes / 60.

% --------------------------------------
% predicates that initiates the deliveries array, in the form (StartTime - EndTime - IDTruck - IDSource)
initDeliveriesArray(DeliveriesList, Length) :-
    initDeliveriesArrayAux(DeliveriesList, 0, Length).

initDeliveriesArrayAux([], Length, Length) :- !.

initDeliveriesArrayAux([(StartTime-EndTime-IDTruck-IDSource) | Rest], N, Length) :-
    Next is N + 1,
    initDeliveriesArrayAux(Rest, Next, Length).



% --------------------------------------
% 1A PARTE: TODAS AS FARMACIAS DISPONIVEIS, CARRINHA COM CAPACIDADE INFINITA
parte1(DistancesArray, List, Dist) :- % por agora so recebo distances array
    countNumLocals(DistancesArray, [], 1, NumLocals), % conta o numero de locais (farmacias + central)
    length(List, NumLocals),
    domain(List, 1, NumLocals),
    constrain_dists(List, 1, Dist, DistancesArray),
    circuit(List), % faz circuito
    labeling([minimize(Dist)], List).


constrain_dists([], _, 0, _).

constrain_dists([Head | Tail], Count, Dist, DistancesArray) :-
    NewCount is Count + 1,
    constrain_dists(Tail, NewCount, RestDistance, DistancesArray),
    member((Count-Head-Distance), DistancesArray), % erro esta no distance
    Dist #= Distance + RestDistance. % distancia de A para B, mais 30 mins para descarregar, mais o resto da distancia


countNumLocals(DistancesArray, Buff, Temp, NumLocals) :-
    member((1-D-_), DistancesArray),
    \+ member(D, Buff), !,
    NewTemp is Temp + 1,
    countNumLocals(DistancesArray, [D | Buff], NewTemp, NumLocals).

countNumLocals(_, _, N, N).