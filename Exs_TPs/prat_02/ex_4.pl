fatorial(0, 1).
fatorial(1, 1).

fatorial(N, V):-
    N > 1,
    N1 is N - 1,
    fatorial(N1, V1),
    V is N * V1.

fibonacciRec(0,1).
fibonacciRec(1,1).
fibonacciRec(N,F) :-
    N > 1,
    N_prev is N-1, fibonacciRec(N_prev, F_prev),
    N_prev2 is N-2, fibonacciRec(N_prev2, F_prev2),
    F is F_prev + F_prev2.

fibonacciTail(0,0).
fibonacciTail(1,1).
fibonacciTail(N,Result) :- fibTailAux(N,0,1,Result).

fibTailAux(0,N,_,N).
fibTailAux(N, Prev1, Prev2, Result) :-
    N > 0,
    NewPrev2 is Prev1+Prev2,
    N1 is N-1,
    fibTailAux(N1,Prev2,NewPrev2,Result).
