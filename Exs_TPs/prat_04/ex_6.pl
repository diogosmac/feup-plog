:- use_module(library(lists)).

ligacao(1, 2).
ligacao(1, 3).
ligacao(2, 4).
ligacao(3, 4).
ligacao(3, 6).
ligacao(4, 6).
ligacao(5, 6). 

% feito com uma pesquisa em profundidade
% caminho(NoInicio, NoFim, Caminho) :-
%     callProf([NoInicio], NoInicio, NoFim, Caminho).

% callProf(Caminho, NoFim, NoFim, Caminho).

% callProf(Caminho, NoInicio, NoFim, CaminhoFinal) :-
%     ligado(NoInicio, NoIntermedio),
%     (\+ member(NoIntermedio, Caminho)),
%     append(Caminho, NoIntermedio, NovoCaminho),
%     callProf(NovoCaminho, NoIntermedio, NoFim, CaminhoFinal).

% callProf(Caminho, NoInicio, NoFim, CaminhoFinal) :-
%     ligado(NoIntermedio, NoInicio),
%     (\+ member(NoIntermedio, Caminho)),
%     append(Caminho, NoIntermedio, NovoCaminho),
%     callProf(NovoCaminho, NoIntermedio, NoFim, CaminhoFinal).

ligacao2(X,Y):- ligacao(X,Y).
ligacao2(X,Y):- ligacao(Y,X). 

caminho(NoInicio, NoFim, Lista):-
caminho(NoInicio, NoFim, [NoInicio], Lista, 5).
caminho(NoInicio, NoFim, Lista, ListaFim,_):-
 ligacao2(NoInicio, NoFim),
 append(Lista, [NoFim], ListaFim).
caminho(NoInicio, NoFim, Lista, ListaFim, N):-
 N>0,
 ligacao2(NoInicio, NoInterm),
 NoInterm \= NoFim,
 \+(member(NoInterm, Lista)),
 append(Lista, [NoInterm], Lista2),
 N2 is N-1,
 caminho(NoInterm, NoFim, Lista2, ListaFim, N2).