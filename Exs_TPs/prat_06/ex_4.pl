functor_(Term,F,N):- Term=..[F|Args], length(Args,N).

arg2(N, Term, Arg) :-
    Term =.. [Name | ArgsList],
    argAux(N, ArgsList, Arg).

argAux(1, [Argument | Rest], Argument).

argAux(N, [Argument | Rest], Arg) :-
    N > 1,
    Next is N - 1,
    argAux(Next, Rest, Arg).