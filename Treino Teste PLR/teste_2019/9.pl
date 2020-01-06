:- use_module(library(clpfd)).
:- use_module(library(lists)).

objeto(piano, 3, 30).
objeto(cadeira, 1, 10).
objeto(cama, 3, 15).
objeto(mesa, 2, 15).
homens(4).
tempo_max(60).

move_objetos :-
    le_input(NomeObjetos, HomensNecessarios, TempoObjetos, NumHomens, TempoMax),
    length(NomeObjetos, NumObjetos),
    length(StartTimeObjetos, NumObjetos),
    length(EndTimeObjetos, NumObjetos),
    domain(StartTimeObjetos, 0,  TempoMax),
    domain(EndTimeObjetos, 0, TempoMax),

    generate_tasks(HomensNecessarios, TempoObjetos, StartTimeObjetos, EndTimeObjetos, 1, Tasks),
    cumulative(Tasks, [limit(NumHomens)]),
    maximum(End, EndTimeObjetos),
    End #=< TempoMax,

    append(StartTimeObjetos, EndTimeObjetos, AllVars),
    labeling([minimize(End)], AllVars),

    nl, output(NomeObjetos, StartTimeObjetos, EndTimeObjetos, End).


output(NomeObjetos, StartTimeObjetos, EndTimeObjetos, End) :-
    write('Total time: '), write(End), nl, nl,
    output_aux(NomeObjetos, StartTimeObjetos, EndTimeObjetos).

output_aux([], [], []).
output_aux([Nome | NomeObjetos], [StartTime | StartTimeObjetos], [EndTime | EndTimeObjetos]) :-
    write(Nome), write(': '), write(StartTime), write(' - '), write(EndTime), nl,
    output_aux(NomeObjetos, StartTimeObjetos, EndTimeObjetos).



generate_tasks([], [], [], [], _, []).
generate_tasks([Recursos | HomensNecessarios], [Duracao | TempoObjetos], [StartTime | StartTimeObjetos], [EndTime | EndTimeObjetos],
               Counter, [task(StartTime, Duracao, EndTime, Recursos, Counter) | Tasks]) :-
            
            NewCounter is Counter + 1,
            generate_tasks(HomensNecessarios, TempoObjetos, StartTimeObjetos, EndTimeObjetos, NewCounter, Tasks).


le_input(NomeObjetos, HomensNecessarios, TempoObjetos, NumHomens, TempoMax) :-
    le_objetos([], NomeObjetos, HomensNecessarios, TempoObjetos),
    homens(NumHomens),
    tempo_max(TempoMax).

le_objetos(NomeObjetosAux, NomeObjetos, [Homens | HomensNecessarios], [Tempo | TempoObjetos]) :-
    objeto(Nome, Homens, Tempo),
    \+ member(Nome, NomeObjetosAux), !,
    append(NomeObjetosAux, [Nome], NewNomeObjetosAux),
    le_objetos(NewNomeObjetosAux, NomeObjetos, HomensNecessarios, TempoObjetos).

le_objetos(NomeObjetos, NomeObjetos, [], []).