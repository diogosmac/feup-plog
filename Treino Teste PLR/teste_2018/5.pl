:- use_module(library(clpfd)).
:- use_module(library(lists)).

optimal_skating_pairs(MenHeights, WomenHeights, Delta, Pairs) :-
    length(MenHeights, N),
    length(PairsMen, N), % no fundo, so precisamos de saber os pares dos homens (se nao tiver par, fica a 0)
    length(WomenHeights, N2),

    % 0 é se nao tiver par
    domain(PairsMen, 0, N2),

    constrain_distinct(PairsMen),

    constrain_pairs(MenHeights, WomenHeights, PairsMen, Delta, 1, NumPairsMade),

    labeling([maximize(NumPairsMade)], PairsMen),

    get_results(PairsMen, 1, [], Pairs).


constrain_pairs(_, _, [], _, _, 0).
constrain_pairs(MenHeights, WomenHeights, [FinalPair | PairsMen], Delta, Counter, NumPairsMade) :-
    nth1(Counter, MenHeights, HeightOfMan),
    element(PairWoman, WomenHeights, HeightOfWoman),
    (HeightOfMan #>= HeightOfWoman #/\ Delta #> (HeightOfMan - HeightOfWoman) #/\ FinalPair #= PairWoman #/\ PairDone #= 1)
    #\/
    (FinalPair #= 0 #/\ PairDone #= 0),

    NumPairsMade #= PairDone + RestOfPairs,

    NewCounter is Counter + 1,
    constrain_pairs(MenHeights, WomenHeights, PairsMen, Delta, NewCounter, RestOfPairs).


% values have to be different, unless they are 0
constrain_distinct([_]).
constrain_distinct([A | PairsMen]) :-
    constrain_distinct_aux(A, PairsMen),
    constrain_distinct(PairsMen).

constrain_distinct_aux(_, []).
constrain_distinct_aux(A, [B | PairsMen]) :-
    A #= 0 #\/ B#= 0 #\/ A #\= B,
    constrain_distinct_aux(A, PairsMen).


get_results([], _, FinalPairs, FinalPairs).
get_results([CurrentWoman | PairsMen], Counter, Pairs, FinalPairs) :-
    if_then_else((CurrentWoman \= 0),
                 (append(Pairs, [Counter-CurrentWoman], NewPairs)),
                 (NewPairs = Pairs)),
    NewCounter is Counter + 1,
    get_results(PairsMen, NewCounter, NewPairs, FinalPairs).



if_then_else(I, T, _) :- I, !, T.
if_then_else(_, _, E) :- E.