% se fizermos desta maneira, os elementos dentro da lista final irao ser o termo
% tambem, devido a unificacao!

% unificavel([],_,[]).
% unificavel([T|Resto],T1,[T|Resto1]):- 
%     T = T1,
%     unificavel(Resto,T1,Resto1). 

% unificavel([T | Resto], T1, Resto1) :-
%     (\+ T=T1),
%     unificavel(Resto, T1, Resto1).

% --------
% e preciso o cut ali porque se nao estivesse, ao clicar ponto e virgula
% a 2ª clausula poderia passar para a 3ª, num elemento que nao unificasse,
% ficando este na lista.

unificavel([],_,[]).

unificavel([T|Resto],T1,Resto1):-
    (\+ T=T1), !,
    unificavel(Resto,T1,Resto1).

unificavel([T|Resto],T1,[T|Resto1]):- unificavel(Resto,T1,Resto1). 