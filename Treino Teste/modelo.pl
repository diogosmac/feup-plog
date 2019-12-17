:- use_module(library(lists)).

participant(1234, 17, 'Pé coxinho').
participant(3423, 21, 'Programar com os pés').
participant(3788, 20, 'Sing a Bit').
participant(4865, 22, 'Pontes de esparguete').
participant(8937, 19, 'Pontes de pen-drives').
participant(2564, 20, 'Moodle hack').

performance(1234,[120,120,120,120]).
performance(3423,[32,120,45,120]).
performance(3788,[110,2,6,43]).
performance(4865,[120,120,110,120]).
performance(8937,[97,101,105,110]).

% ex_1
madeItThrough(Id) :-
    performance(Id, Array),
    madeItAux(Array).

madeItAux([First | Rest]) :-
    First =:= 120.
    
madeItAux([First | Rest]) :-
    First \= 120,
    madeItAux(Rest).

% ex_2
juriTimes(Participants, JuriMember, Times, Total) :-
    juriTimesAux(Participants, JuriMember, Times, 0, Total).


juriTimesAux([], _, [], Total, Total).
juriTimesAux([Participant | Rest], JuriMember, [NewTime | Times], AuxTotal, Total) :-
    performance(Participant, Array),
    getJuriValue(Array, JuriMember, NewTime),
    NewAuxTotal is AuxTotal + NewTime,
    juriTimesAux(Rest, JuriMember, Times, NewAuxTotal, Total).

getJuriValue([Time | Rest], 1, Time).
getJuriValue([Time | Rest], N, NewTime) :-
    N > 1,
    NewN is N - 1,
    getJuriValue(Rest, NewN, NewTime).


% ex_3
patientJuri(Id) :-
    performance(Participant, Array),
    getJuriValue(Array, Id, Time),
    Time =:= 120,
    performance(OtherParticipant, OtherArray),
    Participant \= OtherParticipant,
    getJuriValue(OtherArray, Id, OtherTime),
    OtherTime =:= 120.


%ex_4
bestParticipant(P1, P2, P) :-
    sumTimes(P1, Sum1),
    sumTimes(P2, Sum2),
    chooseBest(P1, Sum1, P2, Sum2, P).

sumTimes(Id, Sum) :-
    performance(Id, Array),
    sumTimesAux(Array, 0, Sum).

sumTimesAux([], Sum, Sum).
sumTimesAux([Next | Rest], AuxSum, Sum) :-
    NewAuxSum is AuxSum + Next,
    sumTimesAux(Rest, NewAuxSum, Sum).

chooseBest(P1, Sum1, _, Sum2, P) :-
    Sum1 > Sum2,
    P = P1.

chooseBest(_, Sum1, P2, Sum2, P) :-
    Sum1 < Sum2,
    P = P2.


% ex_5
allPerfs :-
    performance(Id, Array),
    participant(Id, _, Performance),
    write(Id), write(':'),
    write(Performance), write(':'),
    write(Array), nl,
    fail.

allPerfs.


% ex_6
nSuccessfulParticipants(T) :-
    successAux([], 0, T).

successAux(AuxIds, AuxT, T) :-
    performance(Id, Array),
    (\+ member(Id, AuxIds)),
    checkArray(Array),
    NewT is AuxT + 1, !,
    successAux([Id | AuxIds], NewT, T).

successAux(_, T, T).


checkArray([]).
checkArray([First | Rest]) :-
    First =:= 120,
    checkArray(Rest).


% ex_7
juriFans(L) :-
    findall(Id-JuriFansArray, juriFansAux(Id, JuriFansArray), L).

juriFansAux(Id, JuriFansArray) :-
    performance(Id, Array),
    getJuriFans(Array, 1, JuriFansArray).

getJuriFans([], _, []).
getJuriFans([Time | Rest], N, [N | JuriFansArray]) :-
    Time =:= 120,
    NextN is N + 1,
    getJuriFans(Rest, NextN, JuriFansArray).

getJuriFans([Time | Rest], N, JuriFansArray) :-
    Time \= 120,
    NextN is N + 1,
    getJuriFans(Rest, NextN, JuriFansArray).


% ex_8
eligibleOutcome(Id,Perf,TT) :-
    performance(Id,Times),
    madeItThrough(Id),
    participant(Id,_,Perf),
    sumlist(Times,TT).

nextPhase(N, Participants) :-
    setof(TT-Id-Perf, eligibleOutcome(Id, Perf, TT), AuxParticipants),
    reverse(AuxParticipants, ReverseAuxParticipants),
    getNResults(ReverseAuxParticipants, N, Participants).

getNResults([Result | More], N, [Result | Participants]) :-
    N >= 1,
    NextN is N - 1,
    getNResults(More, NextN, Participants).

getNResults(_, N, []) :-
    N =:= 0.


% ex_9
predX(Q,[R|Rs],[P|Ps]) :-
    participant(R,I,P), I=<Q,
    predX(Q,Rs,Ps).
predX(Q,[R|Rs],Ps) :-
    participant(R,I,_), I>Q,
    predX(Q,Rs,Ps).
predX(_,[],[]).

% O predicado tem como argumentos uma dada idade, em Q, e uma lista de IDs dos participantes,
% em [R | Rs]. O predicado retorna uma lista com os nomes das performances dos
% participantes da lista cuja idade e menor ou igual a Q.

% O cut e verde uma vez que nao e essencial para o funcionamento do programa,
% apenas "poda" a arvore de pesquisa do predicado pois elimina verificacoes
% inuteis que poderiam ser feitas se houvesse backtracking completo.

% ex_10
impoe(X,L) :-
    length(Mid,X),
    append(L1,[X|_],L), append(_,[X|Mid],L1).

% o predicado verifica se, para um numero X e uma lista L, esta lista contem
% 2 instancias desse numero X, sendo que entre essas duas instancias devem estar
% X elementos. Se L nao for instanciado quando o predicado e chamado ele cria uma lista incompleta.
% É de notar que o predicado apenas faz esta verificacao para o primeiro par de elementos
% X que encontrar (ex: impoe(2, [2, 1, 4, 2, 1, 2]) ira retornar verdadeiro).

% ex_11
langford(N, L) :-
    ListSize is 2 * N,
    length(List, ListSize),
    impoeAux(N, List, L).

impoeAux(0, List, List).

impoeAux(N, List, L) :-
    N > 0,
    impoe(N, List),
    NextN is N - 1,
    impoeAux(NextN, List, L).