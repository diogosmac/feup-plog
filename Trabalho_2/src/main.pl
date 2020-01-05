:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- consult('io.pl').
:- consult('algs.pl').

main :-
    readFiles('large/trucks.txt', TruckCapacityList, NumOfTrucks, 'large/pharmacies.txt', PharmaciesList, 'large/distances.txt', DistancesList),

    part3(DistancesList, TruckCapacityList, NumOfTrucks, PharmaciesList, OrderList, StartTimesList, DeliveriesList, TimeCost, DifferentVehicles),

    print_output(OrderList, StartTimesList, DeliveriesList, TimeCost, DifferentVehicles).
