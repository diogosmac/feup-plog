salto_cavalo(X/Y, Q) :-
    movimento(X/Y, R/T),
    verifica_dentro_tab(R/T),
    Q = R/T.

movimento(X/Y, R/T) :-
    R is X - 2, T is Y - 1.

movimento(X/Y, R/T) :-
    R is X - 1, T is Y - 2.

movimento(X/Y, R/T) :-
    R is X + 2, T is Y - 1.

movimento(X/Y, R/T) :-
    R is X + 1, T is Y - 2.

movimento(X/Y, R/T) :-
    R is X + 2, T is Y + 1.

movimento(X/Y, R/T) :-
    R is X + 1, T is Y + 2.

movimento(X/Y, R/T) :-
    R is X - 2, T is Y + 1.

movimento(X/Y, R/T) :-
    R is X - 1, T is Y + 1.

verifica_dentro_tab(R/T) :-
    R >=1, R =< 8,
    T >=1, T =< 8.

% ---------------------

trajeto_cavalo([First | Rest]) :-
    verifica_trajeto(First, Rest).

verifica_trajeto(_, []).

verifica_trajeto(First, [Next | Rest]) :-
    salto_cavalo(First, Next),
    verifica_trajeto(Next, Rest).
