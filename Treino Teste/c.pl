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
diff(User1, User2, Diff, Film) :-
    vote(User1, Movies1),
    vote(User2, Movies2),
    member(Film-Rating1, Movies1),
    member(Film-Rating2, Movies2),
    Diff is abs(Rating1 - Rating2).


% ex_3
niceGuy(User) :-
    vote(User, Movies),
    member(Movie1-Rating1, Movies),
    Rating1 >= 8,
    member(Movie2-Rating2, Movies),
    Movie1 \= Movie2,
    Rating2 >= 8.


% ex_4
elemsComuns([], [], _).

elemsComuns([New | List1], [New | Common], List2) :-
    member(New, List2), !,
    elemsComuns(List1, Common, List2).

elemsComuns([_ | List1], Common, List2) :-
    elemsComuns(List1, Common, List2).


% ex_5
printCategory(Category) :-
    film(Movie, Cats, Time, Rating),
    member(Category, Cats),
    write(Movie), write(' ('),
    write(Time), write('min, '),
    write(Rating), write('/10)'), nl,
    fail.

printCategory(_).


% ex_6
similarity(Film1, Film2, Similarity) :-
    film(Film1, Cats1, Time1, Rating1),
    film(Film2, Cats2, Time2, Rating2),
    getPercentDiff(Cats1, Cats2, PercentDiff),
    DurDiff is abs(Time1 - Time2),
    ScoreDiff is abs(Rating1 -  Rating2),
    Similarity is PercentDiff - 3 * DurDiff - 5 * ScoreDiff.

getPercentDiff(Cats1, Cats2, PercentDiff) :-
    elemsComuns(Cats1, Common, Cats2),
    length(Common, CommonLen),
    append(Cats1, Cats2, All),
    sort(All, GoodAll),
    length(GoodAll, AllLen),
    PercentDiff is (CommonLen / AllLen) * 100.


% ex_7
mostSimilar(Film, Similarity, Films) :-
    findall(Sim-ListMovies, setof(Movie, similarity(Film, Movie, Sim), ListMovies), FinalList),
    reverse(FinalList, ReversedList),
    getResult(ReversedList, Similarity, Films).

getResult([SameMovie, Sim-Films | Rest], Sim, Films) :-
    Sim > 10, !.

getResult(_, 0, []).


% ex_8
distancia(User1, Distancia, User2) :-
    user(User1, Age1, Country1),
    user(User2, Age2, Country2),
    vote(User1, Movies1),
    vote(User2, Movies2),
    getAvgDiff(Movies1, Movies2, AvgDiff),
    AgeDiff is abs(Age1 - Age2),
    getCountryDiff(Country1, Country2, CountryDiff),
    Distancia is AvgDiff + (AgeDiff / 3) + CountryDiff.

getCountryDiff(Country, Country, 0) :- !.
getCountryDiff(_, _, 2).

getAvgDiff(Movies1, Movies2, AvgDiff) :-
    getAvgDiffAux(Movies1, Movies2, RatingDiffs),
    length(RatingDiffs, Len),
    Len > 0,
    sumlist(RatingDiffs, Sum),
    AvgDiff is Sum / Len.


getAvgDiffAux([], Movies2, []).

getAvgDiffAux([Movie-Rating1 | Rest], Movies2, [Diff | RatingDiffs]) :-
    member(Movie-Rating2, Movies2), !,
    Diff is abs(Rating1 - Rating2),
    getAvgDiffAux(Rest, Movies2, RatingDiffs).

getAvgDiffAux([_ | Rest], Movies2, RatingDiffs) :-
    getAvgDiffAux(Rest, Movies2, RatingDiffs).