:- consult('utility.pl').

le(Prompt, Texto) :-
    write(Prompt), write('>'),
    get0(Ch),
    leResto(Ch, ListaChars),
    name(Texto, ListaChars).

leResto(10,[]).
leResto(13,[]).
leResto(Ch, [Ch | Mais]) :-
    get0(Ch1),
    leResto(Ch1, Mais).

% ---------------------------------------------------------------------

leInt(Prompt, Numero) :-
    write(Prompt), write('> '),
    read(Numero).

% ---------------------------------------------------------------------
% Predicado que pede uma linha e coluna (entre 1 e 7) ao utilizador.
% Linha e Coluna sao ambos retornados pelo predicado

pedeLinhaEColuna(Linha, Coluna) :-
    leInt('Insira o numero de uma linha (1 a 7) ', InLinha),
    leInt('Insira o numero de uma coluna (1 a 7) ', InColuna),
    verificaLinhaEColuna(InLinha, InColuna, Linha, Coluna).

% feito de modo a que os ultimos dois argumentos sejam iguais a inputs validos;
% os primeiros dois sao os inputs que o utilizador da.

verificaLinhaEColuna(InLinha, InColuna, InLinha, InColuna) :-
    InLinha > 0, InLinha < 8,
    InColuna > 0, InColuna < 8.

verificaLinhaEColuna(InLinha, InColuna, Linha, Coluna) :-
    format("~n~nInputs invalidos. Por favor, tente de novo.~n~n", []),
    leInt('Insira o numero de uma linha (1 a 7) ', NewLinha),
    leInt('Insira o numero de uma coluna (1 a 7) ', NewColuna),
    verificaLinhaEColuna(NewLinha, NewColuna, Linha, Coluna).

