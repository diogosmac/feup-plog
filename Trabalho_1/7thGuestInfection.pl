:- use_module(library(lists)).

tabuleiro([[' ', 1, 2, ' ', 1, ' ', ' '],
           [' ', ' ', 2, ' ', ' ', ' ', ' '],
           [' ', ' ', 2, ' ', ' ', ' ', ' '],
           [' ', 1, ' ', 1, 1, ' ', ' '],
           [' ', ' ', 2, ' ', 2, ' ', ' '],
           [' ', ' ', 2, ' ', ' ', ' ', ' '],
           [' ', ' ', 2, ' ', 1, ' ', ' ']
          ]).


% ---------------------------------------------------------------------

% -> leInt(Prompt, Inteiro)

le(Prompt, Texto) :-
    write(Prompt), write(':'),
    get0(Ch),
    leResto(Ch, ListaChars),
    name(Texto, ListaChars).

leResto(10,[]).
leResto(13,[]).
leResto(Ch, [Ch | Mais]) :-
    get0(Ch1),
    leResto(Ch1, Mais).

% ---------------------------------------------------------------------

jogaPeca(Linha, Coluna, Peca, TabIn, TabOut) :-
    atualizaLinha(Linha, Coluna, Peca, TabIn, TabOut).

atualizaLinha(1, Coluna, Peca, [Linha | Mais], [NovaLinha | Mais]) :-
    atualizaColuna(Coluna, Peca, Linha, NovaLinha).

atualizaLinha(N, Coluna, Peca, [Linha | Mais], [Linha | MaisLinhas]) :-
    N > 1,
    Next is N-1,
    atualizaLinha(Next, Coluna, Peca, Mais, MaisLinhas).

atualizaColuna(1, Peca, [_ | Resto], [Peca | Resto]).
atualizaColuna(N, Peca, [P | Resto], [P | Mais]) :-
    N > 1,
    Next is N - 1,
    atualizaColuna(Next, Peca, Resto, Mais).

% ---------------------------------------------------------------------

% cell(x, y, peca).
% atualizaCelula(X, Y, NewX, NewY, Peca) :-
%     retract(cell(X,Y,Peca)).
%     assertz(cell(NewX,NewY,Peca)).
% possível mas lento (also feio, not sexy)

% ---------------------------------------------------------------------

% format aka printf
% format(".. | ~p | ~d | .... | ~n", [el1, el2, ...]).

% ---------------------------------------------------------------------

joga :-
    inicializa,
    repeat,             % até ter sucesso (fimDeJogo)
        joga,
        fimDeJogo,
    mostraVencedor.

repeat.
repeat :-
    repeat.


% ---------------------------------------------------------------------
% Tab - tabuleiro (lista de listas)
% Pontos1 - Numero de pontos do primeiro jogador
% Pontos2 - Numero de pontos do segundo jogador
% Turno - Numero 1 ou 2, de modo a saber quem e o proximo jogador a jogar

imprimeInfo(Tab, Pontos1, Pontos2, Turno) :-
    format("----------------------------------------------~n~n", []),
    imprimeTab(Tab),
    nl,
    format("          Player 1         Player 2           ~n", []),
    format("            ~p                ~p              ~n", [Pontos1, Pontos2]),
    nl,
    format("           E o turno do jogador ~p.           ~n", [Turno]),
    format("----------------------------------------------~n", []).



imprimeTab(Tab) :-
    imprimeTabLinha(Tab).

imprimeTabLinha([]).

imprimeTabLinha([X | RestoLinhas]) :-
    format("|  ~p  |  ~p  |  ~p  |  ~p  |  ~p  |  ~p  |  ~p  |~n~n", X),
    imprimeTabLinha(RestoLinhas).



    