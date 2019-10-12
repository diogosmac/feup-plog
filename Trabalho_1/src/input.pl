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