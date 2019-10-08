divisible(X,Y) :-
    N is Y*Y,
    N =< X,
    X mod Y =:= 0.

divisible(X,Y) :-
    Y < X,
    Y1 is Y+1,
    divisible(X,Y1).

isPrime(X) :-
    Y is 2, X > 1, \+divisible(X,Y).

# -------------- Solution --------------

e_primo(2).
e_primo(3).
e_primo(P) :-
    integer(P),
    P > 3,
    P mod 2 =\= 0, \+tem_factor(P,3).
    # P nao e multiplo de 2 nem de 3

tem_factor(N,L) :- N mod L =:= 0.
# N e multiplo de L
tem_factor(N,L) :- L * L < N, L2 is L + 2, tem_factor(N,L2).
# L*L menor que N (se L ao quadrado for maior que N,
# L não pode ser um fator de N)
# Como tem_factor será chamado para 3, L+2 significa que
# vão ser percorridos todos os fatores ímpares, pois
# para haver um fator par teria o próprio número de ser par,
# condição que já foi verificada
