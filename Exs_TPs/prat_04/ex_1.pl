:- use_module(library(lists)).

ligado(a,b). 
ligado(f,i).
ligado(a,c). 
ligado(b,d).
ligado(b,c). 
ligado(d,c).
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
% Acha todos os X onde Y esta satisfeito e retorna numa lista Y
ache_todos(X, Y, Z):- bagof(X, Y, Z), !.
ache_todos(_, _, []).
% Estende a fila ate um filho N1 de N, verificando se N1
% não pertence à fila, prevenindo, assim, ciclos
estende_ate_filho([N|Trajectoria], [N1,N|Trajectoria]):-
 ligado(N, N1),
 (\+ membro(N1, Trajectoria)).
% Encontra o caminho Solucao entre No_inicial e No_Meta
resolva_larg(No_inicial, No_meta, Solucao):-
 largura([[No_inicial]], No_meta, Sol1),
 inverte(Sol1, Solucao).
% Realiza a pesquisa em largura
largura([[No_meta|T]|_],No_meta,[No_meta|T]).
largura([T|Fila],No_meta,Solucao):-
 ache_todos(ExtensaoAteFilho,estende_ate_filho(T,ExtensaoAteFilho),Extensoes),
 concatena(Fila, Extensoes, FilaExtendida),
 largura(FilaExtendida, No_meta, Solucao). 