:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- consult('io.pl').
:- consult('algs.pl').

main :-
    readFiles('small/trucks.txt', TruckCapacityList, NumOfTrucks, 'small/pharmacies.txt', PharmaciesList, 'small/distances.txt', DistancesList),

    part3(DistancesList, TruckCapacityList, NumOfTrucks, PharmaciesList, OrderList, StartTimesList, DeliveriesList, TimeCost, DifferentVehicles),

    print_output(OrderList, StartTimesList, DeliveriesList, TimeCost, DifferentVehicles).
