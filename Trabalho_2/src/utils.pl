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

% initDeliveriesArrayAux([(StartTime-EndTime-IDTruck-IDSource) | Rest], N, Length) :-
%     Next is N + 1,
%     initDeliveriesArrayAux(Rest, Next, Length).

initDeliveriesArrayAux([(_-_-_-_) | Rest], N, Length) :-
    Next is N + 1,
    initDeliveriesArrayAux(Rest, Next, Length).

% ---------------------------------------
% PART I - REGULAR TSP
part1([First | Rest], List, Dist) :-
    length(First, NumLocals), % see length of first sub-array in order to calculate number of locals (pharmacies + central)
    DistancesList = [First | Rest],
    length(List, NumLocals), 
    domain(List, 1, NumLocals),
    constrain_dists(DistancesList, List, CostList), % returns array with all the distance costs
    sum(CostList, #=, Dist), % sums final times
    circuit(List), % does circuit
    labeling([minimize(Dist)], List). % solves, trying to minimize time/distance


constrain_dists([], [], []).

constrain_dists([Array | DistancesList], [Local | Rest], [NewCost | CostList]) :-
    element(Local, Array, NewCostAux),
    NewCost #= NewCostAux + 30, % distance from point A to B, plus 30 mins for delivery
    constrain_dists(DistancesList, Rest, CostList).

% ---------------------------------------
% PART II - TSP WITH TIME WINDOWS - all times in minutes

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
    Time in StartMinutes .. EndMinutesWithDelivery, % domain of the start time
    NewCount is Count + 1,
    putTimeDomainAux(Rest, NewCount, PharmaciesList).

% ---------------------------------------
% Part III - TSP WITH TIME WINDOWS, WHILE ALLOCATING DELIVERIES TO THE AVAILABLE VEHICLES

% variables:
% - array of sucessors [sc, s2, s3, ..., si] - pharmacy/delivery done after i (c is central)
% - array of start times [tc, t2, t3, ..., ti] - time at which the service begins in pharmacy i (central appears twice)
% - array of deliveries [d1, d2, ..., di] - vehicle which will be assigned to the delivery i

part3([First | Rest], TruckCapacity, NumOfTrucks, PharmaciesList, OrderList, StartTimesList, DeliveriesList, TimeCost) :-
    length(First, NumLocals), % see length of first sub-array in order to calculate number of locals (pharmacies + central)
    DistancesList = [First | Rest],
    length(OrderList, NumLocals), % variable list of orders has length equal to the number of locals 
    NumTimes is NumLocals + 1,
    length(StartTimesList, NumTimes), % variable list of times has length equal to the number of locals + 1 (central appears twice) 

	length(PharmaciesList, NumPharmacies),
	length(DeliveriesList, NumPharmacies), % list with one element for each pharmacy

    domain(OrderList, 1, NumLocals),
    putTimeDomain(StartTimesList, PharmaciesList),
	domain(DeliveriesList, 1, NumOfTrucks),
	ExitIndex is NumLocals + 1,
	generateTasks(PharmaciesList, 2, ExitIndex, DeliveriesList, Tasks),
	generateMachines(1, NumOfTrucks, TruckCapacity, Machines),
	
	cumulatives(Tasks, Machines, [bound(upper)]),

    generate_constraints(DistancesList, 1, StartTimesList, OrderList, CostList, NumTimes), 
	% returns array with all the distance costs, and constraints all the start time

    sum(CostList, #=, TimeCost), % sums final times
	nvalue(DifferentVehicles, DeliveriesList),

	Cost #= TimeCost + (10 * DifferentVehicles),

    circuit(OrderList), % does circuit
	Sol = [OrderList, StartTimesList, DeliveriesList],
    append(Sol, AllVars),
    labeling([minimize(Cost)], AllVars), % solves, trying to minimize time/distance
    fd_statistics.


generateTasks(_, Last, Last, _, []).
generateTasks(PharmaciesList, Counter, ExitIndex, DeliveriesList, [task(1, 1, _, Quantity, DeliveryVehicle) | Tasks]) :-
	member((Counter-_-_-Quantity), PharmaciesList),

	DeliveryPos is Counter - 1,
	element(DeliveryPos, DeliveriesList, DeliveryVehicle),

	NewCounter is Counter + 1,
	generateTasks(PharmaciesList, NewCounter, ExitIndex, DeliveriesList, Tasks).


generateMachines(Last, Last, TruckCapacity, [machine(Last, TruckCapacity)]).
generateMachines(Counter, ExitIndex, TruckCapacity, [machine(Counter, TruckCapacity) | Machines]) :-
	NewCounter is Counter + 1,
	generateMachines(NewCounter, ExitIndex, TruckCapacity, Machines).


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
    Time in StartMinutes .. EndMinutesWithDelivery, % domain of the start time
    NewCount is Count + 1,
    putTimeDomainAux(Rest, NewCount, PharmaciesList).





% ---------------------------------------
% TENTATIVA DE IMPLEMENTAÇÃO DO MODELO TEÓRICO CONCEBIDO PARA A RESOLUÇÃO DO PROBLEMA (VRP-TW)

% vrpTW(TruckCapacity, NumOfTrucks, PharmaciesList, [FirstDistances | Rest],
% 		PredecessorsList, SuccessorsList, PharmacyVehiclesList, 
% 		StartTimesList, CapacitiesList, Cost) :-
	
% 	length(FirstDistances, NumLocals),
% 	length(PredecessorsList, NumLocals),
% 	length(SuccessorsList, NumLocals),
% 	length(PharmacyVehiclesList, NumLocals),
% 	length(StartTimesList, NumLocals),
% 	length(CapacitiesList, NumLocals),

% 	nl, write('> Setting all domains:'), nl, nl,
% 	write('Setting domain for predecessors . . .'), nl,
% 	predecessorsDomain(PredecessorsList, NumLocals),
% 	write('Setting domain for successors . . .'), nl,
% 	successorsDomain(SuccessorsList, NumLocals),
% 	write('Setting domain for vehicles . . .'), nl,
% 	pharmacyVehiclesDomain(PharmacyVehiclesList, NumOfTrucks),
% 	write('Setting domain for start times . . .'), nl,
% 	startTimesDomain(StartTimesList, PharmaciesList),
% 	write('Setting domain for capacities . . .'), nl,
% 	capacitiesDomain(CapacitiesList, TruckCapacity),
% 	nl, write('>> All domains set!!'), nl,

% 	DistancesList = [FirstDistances | Rest],

% 	nl, write('> Setting all constraints:'), nl, nl,
% 	write('Constraining difference . . .'), nl,
% 	constrainDifference(PredecessorsList),
% 	constrainDifference(SuccessorsList),
% 	ExitIndex is NumLocals + 1,
% 	write('Constraining coherence . . .'), nl,
% 	constrainCoherence(PredecessorsList, SuccessorsList, 2, ExitIndex),
% 	write('Constraining path . . .'), nl,
% 	constrainPath(PharmacyVehiclesList, PredecessorsList, SuccessorsList, 2, ExitIndex),
% 	extractQuantities(PharmaciesList, QuantitiesList),
% 	write('Constraining capacities . . .'), nl,
% 	constrainCapacity(CapacitiesList, QuantitiesList, PredecessorsList, SuccessorsList, 2, ExitIndex),
% 	write('Constraining times . . .'), nl,
% 	constrainTime(StartTimesList, PredecessorsList, SuccessorsList, DistancesList, 2, ExitIndex),	
% 	nl, write('>> All constraints set!!'), nl,

% 	nvalue(NumRoutes, PharmacyVehiclesList),
% 	getTotalDist(DistancesList, PredecessorsList, 2, ExitIndex, 0, Dist),
% 	Cost #= Dist * (10 + NumRoutes),
	
% 	Vars = [PredecessorsList, SuccessorsList, PharmacyVehiclesList, StartTimesList, CapacitiesList],
% 	append(Vars, Sol),
% 	labeling([minimize(Cost)], Sol).


% predecessorsDomain([Head | PredecessorsList], NumLocals) :-
% 	Head is 0,
% 	domain(PredecessorsList, 1, NumLocals).

% successorsDomain([Head | SuccessorsList], NumLocals) :-
% 	Head is 0,
% 	domain(SuccessorsList, 1, NumLocals).

% pharmacyVehiclesDomain([Head | PharmacyVehiclesList], NumOfTrucks) :-
% 	Head is 0,
% 	domain(PharmacyVehiclesList, 1, NumOfTrucks).

% startTimesDomain([Head | StartTimesList], PharmaciesList) :-
% 	Head is 600,
% 	startTimesDomainAux(StartTimesList, PharmaciesList, 2).

% startTimesDomainAux([], _, _).
% startTimesDomainAux([Head | StartTimesList], PharmaciesList, Counter) :-
% 	member((Counter-StartTime-EndTime-_), PharmaciesList),
% 	NewStart is floor(StartTime * 60),
% 	NewEnd is floor(EndTime * 60) - 30,
% 	Head in NewStart .. NewEnd,
% 	NewCounter is Counter + 1,
% 	startTimesDomainAux(StartTimesList, PharmaciesList, NewCounter).

% capacitiesDomain([Head | CapacitiesList], TruckCapacity) :-
% 	Head is 0,
% 	domain(CapacitiesList, 1, TruckCapacity).


% constrainDifference([_]).
% constrainDifference([H | PredecessorsList]) :-
% 	iterateAux(H, PredecessorsList),
% 	constrainDifference(PredecessorsList).

% iterateAux(_, []).
% iterateAux(Val, [H|T]) :-
% 	Val #= 0 #\/ (Val #\= 0 #/\ Val #\= H),
% 	iterateAux(Val, T).


% constrainCoherence(_, _, Last, Last).
% constrainCoherence(PredecessorsList, SuccessorsList, Counter, ExitIndex) :-
% 	element(Counter, PredecessorsList, Pi1),
% 	element(Pi1, SuccessorsList, Si1),
% 	Si1 #= 0 #\/ (Si1 #\= 0 #/\ Si1 #= Counter),
	
% 	element(Counter, SuccessorsList, Si2),
% 	element(Si2, PredecessorsList, Pi2),
% 	Pi2 #= 0 #\/ (Pi2 #\= 0 #/\ Pi2 #= Counter),

% 	NewCounter is Counter + 1,
% 	constrainCoherence(PredecessorsList, SuccessorsList, NewCounter, ExitIndex).


% constrainPath(_, _, _, Last, Last).
% constrainPath(PharmacyVehiclesList, PredecessorsList, SuccessorsList, Counter, ExitIndex) :-
% 	element(Counter, PharmacyVehiclesList, Vi),
	
% 	element(Counter, PredecessorsList, Pi),
% 	element(Pi, PharmacyVehiclesList, Vpi),
% 	Pi #= 0 #\/ (Vpi #\= 0 #/\ Vpi #= Vi),
	
% 	element(Counter, SuccessorsList, Si),
% 	element(Si, PharmacyVehiclesList, Vsi),
% 	Si #= 0 #\/ (Vsi #\= 0 #/\ Vsi #= Vi),

% 	NewCounter is Counter + 1,
% 	constrainPath(PharmacyVehiclesList, PredecessorsList, SuccessorsList, NewCounter, ExitIndex).


% extractQuantities([], []).
% extractQuantities([(_-_-_-Quantity) | Tail], [Quantity | QuantitiesList]) :-
% 	extractQuantities(Tail, QuantitiesList).

% constrainCapacity(_, _, _, _, Last, Last).
% constrainCapacity(CapacitiesList, QuantitiesList, PredecessorsList, SuccessorsList, Counter, ExitIndex) :-
% 	nth1(Counter, QuantitiesList, Ri),
% 	element(Counter, CapacitiesList, Qi),

% 	element(Counter, PredecessorsList, Pi),
% 	element(Pi, CapacitiesList, Qpi),
% 	Qi #= Qpi + Ri,

% 	element(Counter, SuccessorsList, Si),
% 	element(Si, CapacitiesList, Qsi),
% 	element(Si, QuantitiesList, Rsi),
% 	Si #= 0 #\/ (Si #\= 0 #/\ Qsi #= Qi + Rsi),

% 	NewCounter is Counter + 1,
% 	constrainCapacity(CapacitiesList, QuantitiesList, PredecessorsList, SuccessorsList, NewCounter, ExitIndex).


% getDistance(DistancesList, ID1, ID2, Dist) :-
% 	nth1(ID1, DistancesList, List),
% 	element(ID2, List, Dist).

% constrainTime(_, _, _, _, Last, Last).
% constrainTime(StartTimesList, PredecessorsList, SuccessorsList, DistancesList, Counter, ExitIndex) :-
% 	element(Counter, StartTimesList, Ti),

% 	element(Counter, PredecessorsList, Pi),
% 	element(Pi, StartTimesList, Tpi),
% 	getDistance(DistancesList, Counter, Pi, DistIPi),
% 	Ti #>= Tpi + DistIPi + 30,
	
% 	element(Counter, SuccessorsList, Si),
% 	element(Si, StartTimesList, Tsi),
% 	getDistance(DistancesList, Counter, Si, DistISi),
% 	Ti #=< Tsi - DistISi - 30,

% 	NewCounter is Counter + 1,
% 	constrainTime(StartTimesList, PredecessorsList, SuccessorsList, DistancesList, NewCounter, ExitIndex).

% getTotalDist(_, _, Last, Last, Dist, Dist).
% getTotalDist(DistancesList, PredecessorsList, Counter, ExitIndex, Dist, FinalDist) :-
% 	element(Counter, PredecessorsList, Pi),
% 	getDistance(Counter, Pi, Increment),
% 	NewDist #= Dist + Increment,
% 	NewCounter is Counter + 1,
% 	getTotalDist(DistancesList, PredecessorsList, NewCounter, ExitIndex, NewDist, FinalDist).
