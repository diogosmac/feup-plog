:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- consult('io.pl').
:- consult('utils.pl').
:- consult('test.pl').

main :-
    readFiles('small/trucks.txt', TruckCapacity, NumOfTrucks, 'small/pharmacies.txt', PharmaciesList, 'small/distances.txt', DistancesList),

    length(PharmaciesList, Length),
    initDeliveriesArray(DeliveriesList, Length),
    write(DeliveriesList).