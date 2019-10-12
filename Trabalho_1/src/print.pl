% Tab - tabuleiro (lista de listas)
% Pontos1 - Numero de pontos do primeiro jogador
% Pontos2 - Numero de pontos do segundo jogador
% Turno - Letra A ou B, de modo a saber quem e o proximo jogador a jogar

tabuleiro([[' ', a, b, ' ', a, ' ', ' '],
           [' ', ' ', b, ' ', ' ', ' ', ' '],
           [' ', ' ', b, ' ', ' ', ' ', ' '],
           [' ', a, ' ', a, a, ' ', ' '],
           [' ', ' ', b, ' ', b, ' ', ' '],
           [' ', ' ', b, ' ', ' ', ' ', ' '],
           [' ', ' ', b, ' ', a, ' ', ' ']
          ]).

imprimeInfo(Tab, Pontos1, Pontos2, Turno) :-
    format("----------------------------------------------~n~n", []),
    imprimeTab(Tab),
    nl,
    format("          Player A         Player B           ~n", []),
    format("            ~p                ~p              ~n", [Pontos1, Pontos2]),
    nl,
    format("           E o turno do jogador ~p.           ~n", [Turno]),
    format("----------------------------------------------~n", []).



imprimeTab(Tab) :-
    format("     |  1  |  2  |  3  |  4  |  5  |  6  |  7  |~n", []),
    format("-----|-----|-----|-----|-----|-----|-----|-----|~n", []),
    imprimeTabLinha(1, Tab).

imprimeTabLinha(_, []).

imprimeTabLinha(NumLinha, [X | RestoLinhas]) :-
    format("  ~d  ", [NumLinha]),
    format("|  ~p  |  ~p  |  ~p  |  ~p  |  ~p  |  ~p  |  ~p  |~n", X),
    format("-----| --- | --- | --- | --- | --- | --- | --- |~n", []),
    NewNumLinha is NumLinha + 1,
    imprimeTabLinha(NewNumLinha, RestoLinhas).

