:- use_module(library(clpfd)).
:- use_module(library(lists)).

primeiroCarro(Cars, Size) :-
    Cars = [Amarelo, Verde, Azul, Preto], % ordem dos carros
    Size = [SizeAm, SizeVe, SizeAz, SizePr], % tamanho dos carros
    domain(Cars, 1, 4),
    domain(Size, 1, 4),
    all_distinct(Cars),
    all_distinct(Size),

    Azul #\= 4, Azul #\= 1, % ha carros antes e depois do azul
    CarroDepois #= Azul + 1, CarroAntes #= Azul - 1,
    TamCarroAntes #=< TamCarroDepois,
    (Amarelo #= CarroAntes #/\ SizeAm #= TamCarroAntes #/\ Verde #= CarroDepois #/\ SizeVe #= TamCarroDepois)
    #\/
    (Verde #= CarroAntes #/\ SizeVe #= TamCarroAntes #/\ Amarelo #= CarroDepois #/\ SizeAm #= TamCarroDepois)
    #\/
    (Amarelo #= CarroAntes #/\ SizeAm #= TamCarroAntes #/\ Preto #= CarroDepois #/\ SizePr #= TamCarroDepois)
    #\/
    (Preto #= CarroAntes #/\ SizePr #= TamCarroAntes #/\ Amarelo #= CarroDepois #/\ SizeAm #= TamCarroDepois)
    #\/
    (Verde #= CarroAntes #/\ SizeVe #= TamCarroAntes #/\ Preto #= CarroDepois #/\ SizePr #= TamCarroDepois)
    #\/
    (Preto #= CarroAntes #/\ SizePr #= TamCarroAntes #/\ Verde #= CarroDepois #/\ SizeVe #= TamCarroDepois),
    
    minimum(SizeVe, Size),
    Verde #>= Azul,
    Amarelo #>= Preto,
    append(Cars, Size, AllVars),

    labeling([], AllVars).

% o primeiro carro e preto!