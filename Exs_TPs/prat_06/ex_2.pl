separa(L, Pred, List) :-
    separaAux(L, Pred, ListValidos, ListaInvalidos),
    adiciona(ListValidos, ListaInvalidos, List).

separaAux([], _, [], []).

separaAux([X | Resto], Pred, [X | ListaValidos], ListaInvalidos) :-
    Function =.. [Pred, X],
    Function,
    !,
    separaAux(Resto, Pred, ListaValidos, ListaInvalidos).

separaAux([X | Resto], Pred, ListaValidos, [X | ListaInvalidos]) :-
    separaAux(Resto, Pred, ListaValidos, ListaInvalidos).


adiciona([], [], []).

adiciona([], [X | ListaInvalidos], [X | List]) :-
    adiciona([], ListaInvalidos, List).

adiciona([X | ListaValidos], ListaInvalidos, [X | List]) :-
    adiciona(ListaValidos, ListaInvalidos, List).



pred(X) :- X > 2.

