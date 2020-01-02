:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- consult('io.pl').
:- consult('utils.pl').
:- consult('test.pl').

main :-
    readFiles('small/trucks.txt', TruckCapacity, NumOfTrucks, 'small/pharmacies.txt', PharmaciesList, 'small/distances.txt', DistancesList),

    length(PharmaciesList, Length),
    initDeliveriesArray(DeliveriesList, Length),

    part2(DistancesList, PharmaciesList, OrderList, StartTimesList, Cost),

    nl, write('----------------------'), nl, nl,

    write('Visits pharmacies by order '), write(OrderList), nl,
    write('The start times are '), print_start_times(StartTimesList), nl,
	write('Spends a total of '), write(Cost), write(' minutes in trips').