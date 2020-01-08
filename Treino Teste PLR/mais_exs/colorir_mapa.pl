:- use_module(library(clpfd)).
:- use_module(library(lists)).

paises(5).
cores(4,[2,1,1,2]).
 
fronteira(1,2).
fronteira(1,3).
fronteira(1,4).
fronteira(1,5).
fronteira(2,3).
fronteira(2,4).
fronteira(3,4).
fronteira(4,5).
 
exclui(1, [3,4]).
exclui(2, [3]).
seentao(1-4,2-3).
seexclui(1-3,2-1).
diferentes(2,5).
 
preferencias([1-[1,2], 2-[1], 3-[3,4], 4-[1,2]]).


colorir_mapa :-
    paises(NumPaises),
    cores(NumCores, CoresNumVezes),
    length(CoresPaises, NumPaises),
    domain(CoresPaises, 1, NumCores),

    restringe_cor_vezes(CoresPaises, CoresNumVezes, 1),
    
    le_fronteiras([], Fronteiras),
    restringe_fronteiras(Fronteiras, CoresPaises),

    restringe_excluis([], CoresPaises),
    restringe_seentaos([], CoresPaises),
    restringe_seexcluis([], CoresPaises),
    restringe_diferentes([], CoresPaises),
    preferencias(ListaPreferencias),
    restringe_preferencias(ListaPreferencias, CoresPaises, NumPreferenciasFeitas),

    labeling([maximize(NumPreferenciasFeitas)], CoresPaises).



restringe_preferencias([], _, 0).
restringe_preferencias([Pais-ListaPrefs | ListaPreferencias], CoresPaises, NumPreferenciasFeitas) :-
    element(Pais, CoresPaises, CorPais),
    restringe_prefs_aux(CorPais, ListaPrefs, PrefFeita),
    NumPreferenciasFeitas #= PrefFeita + Resto,
    restringe_preferencias(ListaPreferencias, CoresPaises, Resto).


restringe_prefs_aux(_, [], 0).
restringe_prefs_aux(CorPais, [CorPref | ListaPrefs], PrefFeita) :-
    CorPais #= CorPref #<=> B,
    PrefFeita #= B + Resto,
    restringe_prefs_aux(CorPais, ListaPrefs, Resto).


restringe_diferentes(DiferentesAux, CoresPaises) :-
    diferentes(Pais1, Pais2),
    \+ member(Pais1-Pais2, DiferentesAux), !,
    element(Pais1, CoresPaises, CorPais1),
    element(Pais2, CoresPaises, CorPais2),
    CorPais1 #\= CorPais2,
    restringe_diferentes([Pais1-Pais2 | DiferentesAux], CoresPaises).

restringe_diferentes(_, _).


restringe_seexcluis(SeExcluiAux, CoresPaises) :-
    seexclui(Pais1-Cor1, Pais2-Cor2),
    \+ member(Pais1-Pais2, SeExcluiAux), !,
    element(Pais1, CoresPaises, CorPais1),
    element(Pais2, CoresPaises, CorPais2),
    (CorPais1 #= Cor1) #=> (CorPais2 #\= Cor2),
    restringe_seexcluis([Pais1-Pais2 | SeExcluiAux], CoresPaises).

restringe_seexcluis(_, _).


restringe_seentaos(SeEntaoAux, CoresPaises) :-
    seentao(Pais1-Cor1, Pais2-Cor2),
    \+ member(Pais1-Pais2, SeEntaoAux), !,
    element(Pais1, CoresPaises, CorPais1),
    element(Pais2, CoresPaises, CorPais2),
    (CorPais1 #= Cor1) #=> (CorPais2 #= Cor2),
    restringe_seentaos([Pais1-Pais2 | SeEntaoAux], CoresPaises).

restringe_seentaos(_, _).

restringe_excluis_aux(_, []).
restringe_excluis_aux(CorPais, [Cor | ListaCores]) :-
    CorPais #\= Cor,
    restringe_excluis_aux(CorPais, ListaCores).

restringe_excluis(ListaExcluisAux, CoresPaises) :-
    exclui(Pais, ListaCores),
    \+ member(Pais, ListaExcluisAux), !,
    element(Pais, CoresPaises, CorPais),
    restringe_excluis_aux(CorPais, ListaCores),
    restringe_excluis([Pais | ListaExcluisAux], CoresPaises).

restringe_excluis(_, _).


restringe_fronteiras([], _).
restringe_fronteiras([Pais1-Pais2 | Fronteiras], CoresPaises) :-
    element(Pais1, CoresPaises, CorPais1),
    element(Pais2, CoresPaises, CorPais2),
    CorPais1 #\= CorPais2,
    restringe_fronteiras(Fronteiras, CoresPaises).


le_fronteiras(FronteirasAux, Fronteiras) :-
    fronteira(Pais1, Pais2),
    \+ member(Pais1-Pais2, FronteirasAux), !,
    append(FronteirasAux, [Pais1-Pais2], NewFronteirasAux),
    le_fronteiras(NewFronteirasAux, Fronteiras).

le_fronteiras(Fronteiras, Fronteiras).


restringe_cor_vezes(_, [], _).
restringe_cor_vezes(CoresPaises, [CorAtual | CoresNumVezes], Counter) :-
    restringe_uma_cor(CoresPaises, CorAtual, Counter),
    NewCounter is Counter + 1,
    restringe_cor_vezes(CoresPaises, 0, CoresNumVezes, NewCounter).


restringe_uma_cor([], Acc, CoresNumVezes, _) :- CoresNumVezes #>= Acc.
restringe_uma_cor([CorPais | CoresPaises], Acc, CoresNumVezes, IndiceCor) :-
    CorPais #= IndiceCor #<=> B,
    NewAcc #= Acc + B,
    restringe_uma_cor(CoresPaises, NewAcc, CoresNumVezes, IndiceCor).