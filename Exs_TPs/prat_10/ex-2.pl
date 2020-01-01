:-use_module(library(clpfd)).

cart:-
	%Values in list are house numbers and position in list is order visited by
	length(Houses, 10),	
    domain(Houses, 1, 10), 
    all_distinct(Houses),
	element(10, Houses, 6),
	dist(Houses, Dist),
	labeling([maximize(Dist)], Houses).


dist([_], 0).
dist([A,B|T], D):-
    Dist #= RestoDistancia + abs(A - B),
	dist([ B | T ], RestoDistancia).