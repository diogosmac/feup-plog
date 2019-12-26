testInputs :-
    readFiles('small/trucks.txt', TruckCapacity, NumOfTrucks, 'small/pharmacies.txt', PharmaciesList, 'small/distances.txt', DistancesList),

    nl,
    write('TruckCapacity: '), write(TruckCapacity), nl,
    write('NumOfTrucks: '), write(NumOfTrucks), nl, nl,

    writeOutput(PharmaciesList),
    writeOutput2(DistancesList).

writeOutput([]).

writeOutput([(ID-StartTime-EndTime-Volume) | Rest]) :-
    write('ID: '), write(ID),
    write(' ; StartTime: '), write(StartTime),
    write(' ; EndTime: '), write(EndTime),
    write(' ; Volume: '), write(Volume), nl,
    writeOutput(Rest).


writeOutput2([]).

writeOutput2([(ID1-ID2-Time) | Rest]) :-
    write('ID1: '), write(ID1),
    write(' ; ID2: '), write(ID2),
    write(' ; Time: '), write(Time), nl,
    writeOutput2(Rest).