# -> leInt(Prompt, Inteiro)

le(Prompt, Texto) :-
    write(Prompt), write(':'),
    get0(ch),
    leResto(ch, ListaChars),
    name(Texto, ListaChars).

leResto(10,[]).
leResto(13,[]).
leResto(ch, [ch | Mais]) :-
    get0(ch1),
    leResto(ch1, Mais).

# ---------------------------------------------------------------------

jogaPeca(Linha, Coluna, Peca, TabIn, TabOut) :-
    atualizaLinha(Linha, Coluna, Peca, TabIn, TabOut).

atualizaLinha(1, Coluna, Peca, [Linha | Mais], [NovaLinha | Mais]) :-
    atualizaColuna(Coluna, Peca, Linha, NovaLinha).

atualizaLinha(N, Coluna, Peca, [Linha | Mais], [Linha | MaisLinhas]) :-
    N > 1
    Next is N-1,
    atualizaLinha(Next, Coluna, Peca, Mais, MaisLinhas).

atualizaColuna(1, Peca, [_ | Resto], [Peca | Resto]).
atualizaColuna(N, Peca, [P | Resto], [P | Mais]) :-
    N > 1
    Next is N - 1,
    atualizaColuna(Next, Peca, Resto, Mais).

# ---------------------------------------------------------------------

cell(x, y, peca).
atualizaCelula(X, Y, NewX, NewY, Peca) :-
    retract(cell(X,Y,Peca)).
    assertz(cell(NewX,NewY,Peca)).
# possível mas lento (also feio, not sexy)

# ---------------------------------------------------------------------

# format aka printf
format(".. | ~p | ~d | .... | ~n", [el1, el2, ...]).

# ---------------------------------------------------------------------

joga :-
    inicializa,
    repeat,             # até ter sucesso (fimDeJogo)
        joga,
        fimDeJogo,
    mostraVencedor.
