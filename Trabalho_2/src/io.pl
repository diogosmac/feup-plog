:- use_module(library(lists)).

% --------------------------------------
% predicate that will read from the files all the input needed
readFiles(TrucksFile, TruckCapacityList, NumOfTrucks, PharmaciesFile, PharmaciesList, DistancesFile, DistancesList) :-
    % read the truck capacities and the number of trucks from the trucks file
    open(TrucksFile, read, Stream),
    readTrucksFile(Stream, TruckCapacityList, NumOfTrucks),
    close(Stream),

    % generate a tuple (StartTime-EndTime-Volume) for each pharmacy, reading the pharmacies file
    open(PharmaciesFile, read, Stream2),
    readPharmaciesFile(Stream2, PharmaciesList),
    close(Stream2),

    % read all distances from distances file
    open(DistancesFile, read, Stream3),
    readDistancesFile(Stream3, DistancesList),
    close(Stream3),

    !.

% --------------------------------------
% reads input from truck file
readTrucksFile(Stream, TruckCapacityList, NumOfTrucks) :-
    \+ at_end_of_stream(Stream),
    read(Stream, numTrucks(NumOfTrucks)),
    readTrucksFileAux(Stream, TruckCapacityList).

readTrucksFileAux(Stream, []) :-
    at_end_of_stream(Stream), !.

readTrucksFileAux(Stream, [TruckCap | Rest]) :-
    read(Stream, truck(TruckCap)),
    readTrucksFileAux(Stream, Rest).

% --------------------------------------
% reads input from pharmacies file
readPharmaciesFile(Stream, []) :-
    at_end_of_stream(Stream), !.

readPharmaciesFile(Stream, [(ID-StartTime-EndTime-Volume) | Rest]) :-
    \+ at_end_of_stream(Stream),
    read(Stream, pharmacy(ID, StartTime, EndTime, Volume)),
    readPharmaciesFile(Stream, Rest).

% --------------------------------------
% reads input from distances file
readDistancesFile(Stream, []) :-
    at_end_of_stream(Stream), !.

readDistancesFile(Stream, [DistancesArray | Rest]) :-
    \+ at_end_of_stream(Stream),
    read(Stream, distance(DistancesArray)),
    readDistancesFile(Stream, Rest).


% --------------------------------------
% writes minutes value in hours, in a readable format
write_minutes(TotalMinutes) :-
    Hours is TotalMinutes // 60,
    Minutes is TotalMinutes mod 60,
    if_then_else((Hours >= 10), (write(Hours)), (write('0'), write(Hours))), 
    write(':'), 
    if_then_else((Minutes >= 10), (write(Minutes)), (write('0'), write(Minutes))).


if_then_else(C, I, _):- C, !, I.
if_then_else(_, _, E):- E.

% --------------------------------------
% prints all minute values from a list
print_start_times(StartTimesList) :-
    write('['),
    print_start_times_aux(StartTimesList).

print_start_times_aux([LastValue]) :-
    write_minutes(LastValue),
    write(']').

print_start_times_aux([Minutes | StartTimesList]) :-
    write_minutes(Minutes),
    write(','),
    print_start_times_aux(StartTimesList).

% --------------------------------------
% predicates that print, in a readable format, all the output from the main program
print_output_short(OrderList, StartTimesList, DeliveriesList, TimeCost, DifferentVehicles) :-
    write('Output resumed: '), nl, nl,
    write('Visits pharmacies by order '), write(OrderList), nl,
    write('The start times are '), print_start_times(StartTimesList), nl,
	write('Spends a total of '), write(TimeCost), write(' minutes in trips'), nl,
	write('The vehicles assigned to each delivery are '), write(DeliveriesList), nl,
    write(DifferentVehicles), write(' different vehicles were used'), nl, nl.


print_output(OrderList, StartTimesList, DeliveriesList, TimeCost, DifferentVehicles) :-
    nl, write('----------------------'), nl, nl,
    write('Total time spent on trips: '), write(TimeCost), write(' minutes'), nl,
    write(DifferentVehicles), write(' different vehicles were used'), nl, nl,
    print_output_lists(OrderList, StartTimesList, DeliveriesList),
    nl, nl, write('----------------------'), nl, nl,
    print_output_short(OrderList, StartTimesList, DeliveriesList, TimeCost, DifferentVehicles).

print_output_lists([First | OrderList], [FirstTime | StartTimesList], DeliveriesList) :-
    write('Leaving the central (local 1), at time '), write_minutes(FirstTime), write('; leaving for local '), write(First), nl, nl,
    write('-----'), nl, nl,
    print_output_lists_aux(OrderList, StartTimesList, DeliveriesList, 2).

print_output_lists_aux([], [LastTime], [], _) :-
    write('Arriving at central at time '), write_minutes(LastTime).

print_output_lists_aux([NextLocal | OrderList], [StartTime | StartTimesList], [Vehicle | DeliveriesList], Counter) :-
    write('Pharmacy '), write(Counter), write(':'), nl, nl,
    write('Delivery starts at '), write_minutes(StartTime), nl,
    write('Delivery done by vehicle '), write(Vehicle), nl,
    write('Next local in route is '), write(NextLocal), nl, nl,
    write('-----'), nl, nl,
    NewCounter is Counter + 1,
    print_output_lists_aux(OrderList, StartTimesList, DeliveriesList, NewCounter).
