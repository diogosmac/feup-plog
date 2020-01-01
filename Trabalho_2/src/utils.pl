:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- consult('io.pl').

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
% PART 1 - REGULAR TSP
% part1([First | Rest], List, Dist) :-
%     length(First, NumLocals), % see length of first sub-array in order to calculate number of locals (pharmacies + central)
%     DistancesList = [First | Rest],
%     length(List, NumLocals), 
%     domain(List, 1, NumLocals),
%     constrain_dists(DistancesList, List, CostList), % returns array with all the distance costs
%     sum(CostList, #=, Dist), % sums final times
%     circuit(List), % does circuit
%     labeling([minimize(Dist)], List). % solves, trying to minimize time/distance


% constrain_dists([], [], []).

% constrain_dists([Array | DistancesList], [Local | Rest], [NewCost | CostList]) :-
%     element(Local, Array, NewCostAux),
%     NewCost #= NewCostAux + 30, % distance from point A to B, plus 30 mins for delivery
%     constrain_dists(DistancesList, Rest, CostList).

% ---------------------------------------
% PART 2 - TSP WITH TIME WINDOWS - all times in minutes

% variables:
% - array of sucessors [sc, s2, s3, ..., si] - pharmacy/delivery done after i (c is central)
% - array of start times [tc, t2, t3, ..., ti] - time at which the service begins in pharmacy i (central appears twice)

part2([First | Rest], PharmaciesList, OrderList, StartTimesList, Cost) :-
    length(First, NumLocals), % see length of first sub-array in order to calculate number of locals (pharmacies + central)
    DistancesList = [First | Rest],
    length(OrderList, NumLocals), % variable list of orders has length equal to the number of locals 
    NumTimes is NumLocals + 1,
    length(StartTimesList, NumTimes), % variable list of times has length equal to the number of locals + 1 (central appears twice) 

    domain(OrderList, 1, NumLocals),
    putTimeDomain(StartTimesList, PharmaciesList),

    generate_constraints(DistancesList, 1, StartTimesList, OrderList, CostList, NumTimes), % returns array with all the distance costs, and constraints all the start time

    sum(CostList, #=, Cost), % sums final times
    circuit(OrderList), % does circuit
    append(OrderList, StartTimesList, AllVars),
    labeling([minimize(Cost)], AllVars), % solves, trying to minimize time/distance

    fd_statistics.


generate_constraints([], _, _, [], [], _).

generate_constraints([Array | DistancesMatrix], Counter, StartTimesList, [Local | Rest], [NewCost | CostList], NumTimes) :-
    element(Local, Array, NewCost), % distance from point A to B
    nth1(Counter, StartTimesList, Time),
    if_then_else((Counter is 1), 
                 (Time + NewCost #=< StartTimeDestination),
                 (Time + NewCost + 30 #=< StartTimeDestination)),
    % when we leave the central (counter = 1), there is no delivery so we don't need to add the extra half an hour.
    % on every other local, we add 0.5 hours to simulate the delivery, which takes that time to be made.

    (Local #= 1 #/\ TimeLocal #= NumTimes)
    #\/
    (Local #\= 1 #/\ TimeLocal #= Local),
    % if we are restraining the start time of the arrival to the central, restrain the last variable of the list instead of the first,
    % because the first refers to when we leave the central for the first time (always 10 AM)
    element(TimeLocal, StartTimesList, StartTimeDestination),

    NewCounter is Counter + 1,
    generate_constraints(DistancesMatrix, NewCounter, StartTimesList, Rest, CostList, NumTimes).

% nao esquecer de passar para horas os tempos na matriz
% queremos minimizar a distancia/tempo percorrido, logo os 30 mins nao devem ser considerados no Cost da otimizacao

putTimeDomain([Time | Rest], PharmaciesList) :-
    Time is 600, % time at when service starts at central (10 AM always)
    putTimeDomainAux(Rest, 2, PharmaciesList). % pharmacy ID starts at 2 (ID 1 is central)

putTimeDomainAux([LastTime], _, _) :-
    LastTime in 600 .. 1320. % time at when we arrive again at the central (10 AM to 10 PM, in minutes)

putTimeDomainAux([Time | Rest], Count, PharmaciesList) :-
    member((Count-Start-End-_), PharmaciesList),
    StartMinutes is floor(Start * 60),
    EndMinutesWithDelivery is floor(End * 60) - 30, % removing half an hour to the end of the domain, to make time for the delivery (30 minutes)
    % write(StartMinutes), write(' '), write(EndMinutesWithDelivery), nl,
    Time in StartMinutes .. EndMinutesWithDelivery, % domain of the start time
    NewCount is Count + 1,
    putTimeDomainAux(Rest, NewCount, PharmaciesList).
