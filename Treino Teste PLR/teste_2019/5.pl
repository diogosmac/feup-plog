constroi_bins(_, [], []).
constroi_bins(I, [Present | Vars], [B | LBin]) :-
    Present #= I #<=> B,
    constroi_bins(I, Vars, LBin).
