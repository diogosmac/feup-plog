:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- consult('io.pl').
:- consult('utils.pl').
:- consult('test.pl').

main(L, D) :-
    readFiles('small/trucks.txt', TruckCapacity, NumOfTrucks, 'small/pharmacies.txt', PharmaciesList, 'small/distances.txt', DistancesList),

    length(PharmaciesList, Length),
    initDeliveriesArray(DeliveriesList, Length),

    parte1(DistancesList, List, Dist),

    write('Visits pharmacies by order '), write(List), nl,
	write('Spends a total of '), write(Dist), write(' minutes').