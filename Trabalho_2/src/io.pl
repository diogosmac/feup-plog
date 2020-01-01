:- use_module(library(lists)).

% --------------------------------------
% predicate that will read from the files all the input needed
readFiles(TrucksFile, TruckCapacity, NumOfTrucks, PharmaciesFile, PharmaciesList, DistancesFile, DistancesList) :-
    % read the truck capacity and the number of trucks from the trucks file
    open(TrucksFile, read, Stream),
    readTrucksFile(Stream, TruckCapacity, NumOfTrucks),
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
readTrucksFile(Stream, TruckCapacity, NumOfTrucks) :-
    read(Stream, trucks(TruckCapacity, NumOfTrucks)),
    !.

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