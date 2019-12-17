:- dynamic film/4.
:- use_module(library(lists)).

film('Doctor Strange', [action, adventure, fantasy], 115, 7.6).
film('Hacksaw Ridge', [biography, drama, romance], 131, 8.7).
film('Inferno', [action, adventure, crime], 121, 6.4).
film('Arrival', [drama, mystery, scifi], 116, 8.5).
film('The Accountant', [action, crime, drama], 127, 7.6).
film('The Girl on the Train', [drama, mystery, thriller], 112, 6.7).

user(john, 1992, 'USA').
user(jack, 1989, 'UK').
user(peter, 1983, 'Portugal').
user(harry, 1993, 'USA').
user(richard, 1982, 'USA').

vote(john, ['Inferno'-7, 'Doctor Strange'-9, 'The Accountant'-6]).
vote(jack, ['Inferno'-8, 'Doctor Strange'-8, 'The Accountant'-7]).
vote(peter, ['The Accountant'-4, 'Hacksaw Ridge'-7, 'The Girl on the Train'-3]).
vote(harry, ['Inferno'-7, 'The Accountant'-6]).
vote(richard, ['Inferno'-10, 'Hacksaw Ridge'-10, 'Arrival'-9]).

% ex_1
curto(Movie) :-
    film(Movie, _, Time, _),
    Time < 125.


% ex_2
diff(User1, User2, Difference, Film) :-
    vote(User1, Array1), !,
    getVote(Array1, Film, Vote1), % podia ter usado member
    vote(User2, Array2), !,
    getVote(Array2, Film, Vote2),
    Difference is abs(Vote1 - Vote2).

getVote([Film-Rating | _], Film, Rating).
getVote([_ | Rest], Film, Rating) :-
    getVote(Rest, Film, Rating).

% ex_3
niceGuy(User) :-
    vote(User, Array),
    member(Film1-Rating1, Array),
    Rating1 >= 8, !,
    member(Film2-Rating2, Array),
    Film1 \= Film2,
    Rating2 >= 8.

% ex_4
elemsComuns(List1, Common, List2) :-
    commonAux(List1, Common, List2).

commonAux([First1 | Rest1], [First1 | Common], List2) :-
    member(First1, List2), !,
    commonAux(Rest1, Common, List2).

commonAux([_ | Rest1], Common, List2) :-
    commonAux(Rest1, Common, List2).

commonAux([], [], _).


% ex_5
printCategory(Category) :-
    film(Name, Array, Time, Rating),
    member(Category, Array),
    write(Name), write(' ('),
    write(Time), write('min, '),
    write(Rating), write('/10)'),
    nl, fail.

printCategory(_).

% ex_6
similarity(Film1, Film2, Similarity) :-
    film(Film1, Cats1, Time1, Rating1),
    film(Film2, Cats2, Time2, Rating2),
    calcPercentCommonCat(Cats1, Cats2, PercentCommonCat),
    DurDiff is abs(Time1 - Time2),
    ScoreDiff is abs(Rating1 - Rating2),
    Similarity is PercentCommonCat - 3 * DurDiff - 5 * ScoreDiff.

calcPercentCommonCat(Cats1, Cats2, Value) :-
    elemsComuns(Cats1, Common, Cats2),
    length(Common, CommonLen),
    getListAll(Cats1, Cats2, AllAux),
    append(AllAux, Cats2, All),
    length(All, AllLen),
    Value is (CommonLen / AllLen) * 100.


getListAll([Value | Rest], Cats2, [Value | All]) :-
    (\+ member(Value, Cats2)), !,
    getListAll(Rest, Cats2, All).

getListAll([_ | Rest], Cats2, All) :-
    getListAll(Rest, Cats2, All).

getListAll([], _, []).

% ex_7
mostSimilar(Film, Similarity, Films) :-
    findall(Sim-AuxArray, setof(Film2, similarity(Film, Film2, Sim), AuxArray), BigArray),
    reverse(BigArray, FinalArray),
    handleResults(FinalArray, Similarity, Films).

handleResults([SameMovie, Sim-Array | Rest], Sim, Array) :-
    Sim > 10, !.

handleResults(_, 0, []).


% ex_8
distancia(User1, Distance, User2) :-
    vote(User1, Votes1),
    vote(User2, Votes2),
    handleVoteInfo(Votes1, Votes2, 0, SumDiff, 0, NumDiff),
    AvgDiff is SumDiff / NumDiff,
    user(User1, Date1, Country1),
    user(User2, Date2, Country2),
    AgeDiff is abs(Date1 - Date2),
    getCountryDiff(Country1, Country2, CountryDiff),
    Distance is AvgDiff + AgeDiff / 3 + CountryDiff.


handleVoteInfo([], _, SumDiff, SumDiff, NumDiff, NumDiff).
handleVoteInfo([Movie-Rating1 | Rest], Votes2, AuxSumDiff, SumDiff, AuxNumDiff, NumDiff) :-
    member(Movie-Rating2, Votes2), !,
    Diff is abs(Rating1 - Rating2),
    NewAuxSumDiff is AuxSumDiff + Diff,
    NewAuxNumDiff is AuxNumDiff + 1,
    handleVoteInfo(Rest, Votes2, NewAuxSumDiff, SumDiff, NewAuxNumDiff, NumDiff).

handleVoteInfo([_ | Rest], Votes2, AuxSumDiff, SumDiff, AuxNumDiff, NumDiff) :-
    handleVoteInfo(Rest, Votes2, AuxSumDiff, SumDiff, AuxNumDiff, NumDiff).

getCountryDiff(Country, Country, 0) :- !.
getCountryDiff(_, _, 2).


% ex_9
update(Film) :-
    updateAux(Film, [], 0, SumRating, 0, NumRatings),
    Rating is SumRating / NumRatings,
    retract(film(Film, Cats, Time, _)),
    assert(film(Film, Cats, Time, Rating)).


updateAux(Film, AuxRatingsArray, AuxSumRating, SumRating, AuxNumRatings, NumRatings) :-
    vote(Name, Votes),
    member(Film-Rating, Votes),
    (\+ member(Name, AuxRatingsArray)), !,
    NewAuxSumRating is AuxSumRating + Rating,
    NewAuxNumRatings is AuxNumRatings + 1,
    updateAux(Film, [Name | AuxRatingsArray], NewAuxSumRating, SumRating, NewAuxNumRatings, NumRatings).

updateAux(_, _, SumRating, SumRating, NumRatings, NumRatings).


% ex_10
% O predicado em questao calcula, para um dado utilizador U, a media dos ratings que este deu a todos os filmes em que votou.
% O cut e verde, uma vez que nao influencia os resultados do predicado, mas apenas impede algum backtracking desncessario,
% impedindo que o predicado vote() seja chamado outra vez (o que nao vai dar mais solucoes pois o utilizador ja tinha sido encontrado).
% O cut serve entao para "podar" a arvore de pesquisa do predicado e evitar computacoes desnecessarias.
% O cut pode influenciar as respostas se o argumento U nao for dado (neste caso, apenas da o rating para o primeiro utilizador), mas
% assumiu-se que esta nao e a maneira correta de utilizar o predicado, sendo que ao chamar este deve-se especificar um utilizador U.


% ex_11
move(Pos, Celulas) :-
    findall(NewPos, (possibleMove(Pos, NewPos), verifyPos(NewPos)), Celulas).

verifyPos(X/Y) :-
    X > 0, X < 9,
    Y > 0, Y < 9.

possibleMove(X/Y, NewX/NewY) :-
    NewX is X - 2,
    NewY is Y + 1.

possibleMove(X/Y, NewX/NewY) :-
    NewX is X - 2,
    NewY is Y - 1.

possibleMove(X/Y, NewX/NewY) :-
    NewX is X + 2,
    NewY is Y + 1.

possibleMove(X/Y, NewX/NewY) :-
    NewX is X + 2,
    NewY is Y - 1.

possibleMove(X/Y, NewX/NewY) :-
    NewX is X - 1,
    NewY is Y - 2.

possibleMove(X/Y, NewX/NewY) :-
    NewX is X + 1,
    NewY is Y - 2.

possibleMove(X/Y, NewX/NewY) :-
    NewX is X - 1,
    NewY is Y + 2.

possibleMove(X/Y, NewX/NewY) :-
    NewX is X + 1,
    NewY is Y + 2.


% ex_12
podeMoverEmN(Pos, N, Celulas) :-
    moverEmNAux([Pos], N, [], CelulasUnordered),
    sort(CelulasUnordered, Celulas).

moverEmNAux([CurPos | Rest], N, AccCelulas, Celulas) :-
    N > 0,
    move(CurPos, NewCelulas),
    adicionaNovasCelulas(NewCelulas, AccCelulas, NewAccCelulas),
    NewN is N - 1,
    moverEmNAux(NewCelulas, NewN, NewAccCelulas, NewCelulas2),
    moverEmNAux(Rest, N, NewCelulas2, Celulas).

moverEmNAux([], _, Celulas, Celulas).

moverEmNAux(_, 0, Celulas, Celulas).

adicionaNovasCelulas([Pos | Rest], AccCelulas, NewAccCelulas) :-
    (\+ member(Pos, AccCelulas)), !,
    adicionaNovasCelulas(Rest, [Pos | AccCelulas], NewAccCelulas).

adicionaNovasCelulas([_ | Rest], AccCelulas, NewAccCelulas) :-
    adicionaNovasCelulas(Rest, AccCelulas, NewAccCelulas).

adicionaNovasCelulas([], AccCelulas, AccCelulas).

% ex_13
minJogadas(IniPos, FinPos, NumMinJogs) :-
    minJogadasAux(IniPos, FinPos, 1, NumMinJogs).

minJogadasAux(IniPos, FinPos, AuxNumMin, NumMinJogs) :-
    podeMoverEmN(IniPos, AuxNumMin, Array),
    verifyResultsMinJogs(IniPos, FinPos, Array, AuxNumMin, NumMinJogs).

verifyResultsMinJogs(_, FinPos, Array, NumMinJogs, NumMinJogs) :-
    member(FinPos, Array), !.

verifyResultsMinJogs(IniPos, FinPos, _, AuxNumMin, NumMinJogs) :-
    NewAux is AuxNumMin + 1,
    minJogadasAux(IniPos, FinPos, NewAux, NumMinJogs).

