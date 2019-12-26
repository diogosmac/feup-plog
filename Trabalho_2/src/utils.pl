:- use_module(library(lists)).
:- use_module(library(clpfd)).

% --------------------------------------
% predicate that will return the time needed to get from point 1 to point 2, in hours
getTime(P1, P2, Array, Hours) :-
    member((P1-P2-Minutes), Array),
    Hours is Minutes / 60.

% --------------------------------------
% predicates that initiates the deliveries array, in the form (StartTime - EndTime - IDTruck - IDSource)
initDeliveriesArray(DeliveriesList, Length) :-
    initDeliveriesArrayAux(DeliveriesList, 0, Length).

initDeliveriesArrayAux([], Length, Length) :- !.

initDeliveriesArrayAux([(StartTime-EndTime-IDTruck-IDSource) | Rest], N, Length) :-
    Next is N + 1,
    initDeliveriesArrayAux(Rest, Next, Length).