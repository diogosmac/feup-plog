:- use_module(library(lists)).
:- consult('io.pl').

main :-
    readFiles('small/trucks.txt', TruckCapacity, NumOfTrucks, 'small/pharmacies.txt', PharmaciesList, 'small/distances.txt', DistancesList),

    nl,
    write('TruckCapacity: '), write(TruckCapacity), nl,
    write('NumOfTrucks: '), write(NumOfTrucks), nl, nl,

    writeOutput(PharmaciesList).

writeOutput([]).

writeOutput([(StartTime-EndTime-Volume) | Rest]) :-
    write('StartTime: '), write(StartTime),
    write(' ; EndTime: '), write(EndTime),
    write(' ; Volume: '), write(Volume), nl,
    writeOutput(Rest).