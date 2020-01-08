:- use_module(library(clpfd)).
:- use_module(library(lists)).

docente(pedro, m, 3, 6).
docente(joana, f, 3, 4).
docente(ana, f, 2, 5).
docente(joao, m, 2, 4).
docente(david, m, 3, 4).
docente(maria, f, 1, 6).

% 0 equivale a masculino
% 1 equivale a feminino

aulas(ListaStartTimes) :-
    le_docentes([], NomeDocentes, GeneroDocentes, IntervalosDocentes, 0, NumDocentes),
    length(ListaStartTimes, NumDocentes),
    domain(ListaStartTimes, 0, 24),

    generate_tasks(ListaStartTimes, IntervalosDocentes, 1, Tasks),

    cumulative(Tasks), % limite de recursos é 1 por omissão

    get_tempo_mulheres(ListaStartTimes, GeneroDocentes, SomaTemposMulheres),

    labeling([minimize(SomaTemposMulheres)], ListaStartTimes).

    
get_tempo_mulheres([], [], 0).
get_tempo_mulheres([StartTime | ListaStartTimes], [Genero | GeneroDocentes], SomaTemposMulheres) :-
    Genero #= 1 #<=> B,
    SomaTemposMulheres #= (B * StartTime) + Resto,
    get_tempo_mulheres(ListaStartTimes, GeneroDocentes, Resto).


generate_tasks([], [], _, []).
generate_tasks([StartTime | ListaStartTimes], [Start-End | IntervalosDocentes], Counter, [task(StartTime, 1, _, 1, Counter) | Tasks]) :-
    StartTime #>= Start #/\ End #>= StartTime,
    NewCounter is Counter + 1,
    generate_tasks(ListaStartTimes, IntervalosDocentes, NewCounter, Tasks).


le_docentes(NomeDocentesAux, NomeDocentes, [GeneroBinario | GeneroDocentes], [Start-End | IntervalosDocentes], Counter, NumDocentes) :-
    docente(Nome, Genero, Start, End),
    \+ member(Nome, NomeDocentesAux), !,
    if_then_else((Genero == 'm'),
                 (GeneroBinario is 0),
                 (GeneroBinario is 1)),
    append(NomeDocentesAux, [Nome], NewNomeDocentesAux),
    NewCounter is Counter + 1,
    le_docentes(NewNomeDocentesAux, NomeDocentes, GeneroDocentes, IntervalosDocentes, NewCounter, NumDocentes).

le_docentes(NomeDocentes, NomeDocentes, [], [], NumDocentes, NumDocentes).

if_then_else(I, T, _) :- I, !, T.
if_then_else(_, _, E) :- E.