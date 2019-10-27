:- use_module(library(lists)).

% nomesDisp(Dia, ListaNomes) :-
%     verificaDisp(Dia, ListaNomes, []).

% verificaDisp(Dia, ListaNomesValidos, ListaNomesVerificados) :-
%     disponibilidade(Nome, ListaDisps),
%     (\+ member(Nome, ListaNomesVerificados)),
%     append(Nome, ListaNomesVerificados),
%     verificaDispLista(Dia, ListaDisps),
%     append(Nome, ListaNomesValidos),
%     verificaDisp(Dia, ListaNomesValidos, ListaNomesVerificados).
    

% verificaDisp(Dia, ListaNomesValidos, ListaNomesVerificados) :-
%     disponibilidade(Nome, ListaDisps),
%     (\+ member(Nome, ListaNomesVerificados)),
%     append(Nome, ListaNomesVerificados),
%     (\+ verificaDispLista(Dia, ListaDisps)),
%     verificaDisp(Dia, ListaNomesValidos, ListaNomesVerificados).


% verificaDispLista(Dia, [disp(Dia1, Dia2) | Resto]) :-
%     (Dia >= Dia1, Dia =< Dia2);
%     verificaDispLista(Dia, Resto).


% ----- nao funciona! -----
% verificaDispLista(Dia, [X | Resto]) :-
%     (X(Dia1, Dia2), Dia >= Dia1, Dia =< Dia2);
%     verificaDispLista(Dia, Resto).


% -------------------------
disponibilidade(pedro, [disp(2,4), disp(12,20), disp(25,28)]).
disponibilidade(antonio, [disp(3,4), disp(19,22), disp(25,26)]).
disponibilidade(eduardo, [disp(5,10), disp(19,22), disp(23,25)]).
disponibilidade(rui, [disp(6,10), disp(11,14), disp(25,26)]).

% com findall
nomesDisp(Dia, Nomes) :-
    findall(Nome, dispPessoa(Dia, Nome), Nomes).

dispPessoa(Dia, Nome) :-
    disponibilidade(Nome, ListaDisps),
    verificaDispLista(Dia, ListaDisps).

verificaDispLista(Dia, [disp(Dia1, Dia2) | Resto]) :-
    (Dia >= Dia1, Dia =< Dia2);
    verificaDispLista(Dia, Resto).


% sem findall
% nomesDisp(Dia, Nomes) :-
%     nomesDisp(Dia, [], Nomes).

% nomesDisp(Dia, NomesDisps, NomesDispsFinal) :-
%     podeDia(Dia, Nome),
%     (\+ member(Nome, NomesDisps)),
%     nomesDisp(Dia, [Nome | NomesDisps], NomesDispsFinal).

% nomesDisp(_, NomesDisps, NomesDisps).

% podeDia(Dia, Nome) :-
%     disponibilidade(Nome, Disps),
%     member(disp(D1, D2), Disps),
%     Dia >= D1,
%     Dia =< D2.



% b)


% intesetaIntervalo(disp(X1, Y1), disp(X2, Y2), disp(X3, Y3)) :-
