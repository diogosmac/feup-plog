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


% ---------------------------------------
% PARTE 1 - REGULAR TSP
parte1([First | Rest], List, Dist) :-
    length(First, NumLocals), % see length of first sub-array in order to calculate number of locals (pharmacies + central)
    DistancesList = [First | Rest],
    length(List, NumLocals), 
    domain(List, 1, NumLocals),
    constrain_dists(DistancesList, List, CostList), % returns array with all the distance costs
    sum(CostList, #=, Dist), % sums final price
    circuit(List), % does circuit
    labeling([minimize(Dist)], List). % solves, trying to minimize time/distance


constrain_dists([], [], []).

constrain_dists([Array | DistancesList], [Local | Rest], [NewCost | CostList]) :-
    element(Local, Array, NewCostAux),
    NewCost #= NewCostAux + 30, % distance from point A to B, plus 30 mins for delivery
    constrain_dists(DistancesList, Rest, CostList).

% ---------------------------------------
% PARTE 2 - MULTIPLE TSP