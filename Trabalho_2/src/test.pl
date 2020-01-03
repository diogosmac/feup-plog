:- use_module(library(lists)).
:- use_module(library(clpfd)).
% -------------------
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




% ----------------
% carteiro(List, Dist) :-
%     length(List, 10),
%     domain(List, 1, 10), % para saber a ordem  (10 casas)
%     all_distinct(List),
%     write('1'),
%     % sum_dist(List, Dist),
%     write('2'),
%     circuit(List), % faz o trajeto
%     write('labeling'),
%     labeling([maximize(Dist)], List).


% sum_dist(List, Dist) :-
%     sum_dist_aux(List, Dist, Start). % comeca a contar a partir do sucessor do 6

% sum_dist_aux(_, 0, 6).

% sum_dist_aux(List, Dist, Current) :-
%     element(Current, List, Next),
%     Dist #= abs(Next - Current) + RestDist,
%     sum_dist_aux(List, RestDist, Next).

cart:-
	%Values in list are house numbers and position in list is order visited by
	length(Houses, 10),	domain(Houses, 1, 10), all_distinct(Houses),
	element(10, Houses, 6),
	dist(Houses, Dist),
	labeling([maximize(Dist)], Houses),
	write('Visits houses by order '), write(Houses), nl,
	write('Spends a total of '), write(Dist), write(' minutes').


dist([_], 0).
dist([A,B|T], D):-
	dist([B|T], D2),
	D #= D2 + abs(A-B).

schedule(Vs, DifferentVehicles) :-
	Vs= [V1,V2,V3],
	Tasks = [
		task(1, 1, _, 10,  V1),
		task(1, 1, _, 55,  V2),
		task(1, 1, _, 40,  V3)
	],
	Machines = [
		machine(1,100),
		machine(2,100),
		machine(3,100)
	],
	domain(Vs, 1, 3),
	nvalue(DifferentVehicles, Vs),
	cumulatives(Tasks, Machines, [bound(upper)]),
	labeling([minimize(DifferentVehicles)], Vs).
