:- use_module(library(clpfd)).
:- use_module(library(lists)).

gym_pairs(MenHeights, WomenHeights, Delta, Pairs) :-
    length(MenHeights, N),

    length(PairsMen, N),
    length(PairsWomen, N),
    fill_pairs_list(PairsMen, PairsWomen, Pairs),

    domain(PairsMen, 1, N),
    domain(PairsWomen, 1, N),

    all_distinct(PairsMen),
    all_distinct(PairsWomen),

    constrain_pairs(MenHeights, WomenHeights, PairsMen, PairsWomen, Delta, 1),

    append(PairsMen, PairsWomen, AllVars),

    labeling([], AllVars).


fill_pairs_list([], [], []).
fill_pairs_list([Man | PairsMen], [Woman | PairsWomen], [Man-Woman | Rest]) :-
    fill_pairs_list(PairsMen, PairsWomen, Rest).


constrain_pairs(_, _, [], [], _, _).
constrain_pairs(MenHeights, WomenHeights, [Man | PairsMen], [Woman | PairsWomen], Delta, Counter) :-
    Man #= Counter,
    element(Man, MenHeights, HeightOfMan),
    element(Woman, WomenHeights, HeightOfWoman),
    HeightOfMan #>= HeightOfWoman,
    Delta #> (HeightOfMan - HeightOfWoman),
    NewCounter is Counter + 1,
    constrain_pairs(MenHeights, WomenHeights, PairsMen, PairsWomen, Delta, NewCounter).
