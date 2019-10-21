:- use_module(library(lists)).

ligado(a,b). 
ligado(f,i).
ligado(a,c). 
ligado(f,j).
ligado(b,d). 
ligado(f,k).
ligado(b,e). 
ligado(g,l).
ligado(b,f). 
ligado(g,m).
ligado(c,g). 
ligado(k,n).
ligado(d,h). 
ligado(l,o).
ligado(d,i). 
ligado(i,f).


% Pesquisa em profundidade
pesqProf(NoInicial, NoFinal, Solucao) :-
    callProf([NoInicial], NoInicial, NoFinal, Solucao).

callProf(Solucao, NoFinal, NoFinal, Solucao).

callProf(Solucao, NoInicial, NoFinal, SolucaoFinal) :-
    NoInicial \= NoFinal,
    ligado(NoInicial, NoIntermedio),
    (\+ member(NoIntermedio, Solucao)),
    append(Solucao, [NoIntermedio], NovaSolucao),
    callProf(NovaSolucao, NoIntermedio, NoFinal, SolucaoFinal).
 

% Pesquisa em largura
pesqLarg(NoInicial, NoFinal, Solucao) :-
    callLarg([NoInicial], NoInicial, NoFinal, Solucao).

callLarg(Solucao, NoFinal, NoFinal, Solucao.

callLarg(Solucao, NoInicial, NoFinal, SolucaoFinal) :-
    NoInicial \= NoFinal,
    ligado
