:- use_module(library(lists)).

inicial(estado(0, 0)).
final(estado(2, 0)).

operacao(estado(X, Y), estado(0, Y), 'esvaziar balde 1') :- % esvaziar o balde 1
    X =\= 0.

operacao(estado(X, Y), estado(X, 0), 'esvaziar balde 2') :- % esvaziar o balde 2
    Y =\= 0.

operacao(estado(X, Y), estado(4, Y), 'encher balde 1') :- % encher completamente o balde 1
    X =\= 4.

operacao(estado(X, Y), estado(X, 3), 'encher balde 2') :- % encher completamente o balde 2
    Y =\= 3.

operacao(estado(X, Y), estado(Z, 3), 'enche 2 com 1') :- % despejar balde 1 para o 2 até que o 2 fique cheio
    X > 0, Y < 3,
    X >= Y,
    Z is X - Y.

operacao(estado(X, Y), estado(0, Z), 'esvazia 1 para 2') :- % despejar balde 1  para o 2 até que o 1 fique vazio
    X > 0, Y < 3,
    Y > X,
    Z is X + Y.

operacao(estado(X, Y), estado(4, Z), 'enche 1 com 2') :- % despejar balde 2 para o 1 até que o 1 fique cheio
    Y > 0, X < 4,
    X =< Y,
    Z is Y - X.

operacao(estado(X, Y), estado(Z, 0), 'esvazia 2 para 1') :- % despejar balde 2 para o 1 até que o 2 fique vazio
    Y > 0, X < 4,
    Y < X,
    Z is X + Y.


faz(Ef, Ef, [], _).

faz(Ei, Ef, [Op | Operacoes], EstadosAnts) :-
    operacao(Ei, Enext, Op),
    (\+ member(Enext, EstadosAnts)),
    faz(Enext, Ef, Operacoes, [Enext | EstadosAnts]).

baldes :-
    inicial(Ei), final(Ef),
    faz(Ei, Ef, Operacoes, [Ei]),
    reverse(Operacoes, ROperacoes),
    imprimeOps(ROperacoes).

imprimeOps([]).
imprimeOps([Op | Resto]) :-
    write(Op), nl,
    imprimeOps(Resto).