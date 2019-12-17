:- dynamic film/4, user/3, vote/2.
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
raro(Movie) :-
    film(Movie, _, Time, _),
    checkRaro(Time).

checkRaro(Time) :-
    Time < 60.

checkRaro(Time) :-
    Time > 120.


% ex_2
happierGuy(User1, User2, HappierGuy) :-
    getVoteAvg(User1, Avg1),
    getVoteAvg(User2, Avg2),
    checkHappier(User1, Avg1, User2, Avg2, HappierGuy).

checkHappier(User1, Avg1, _, Avg2, User1) :-
    Avg1 > Avg2, !.

checkHappier(_, _, User2, _, User2).

getVoteAvg(User, Avg) :-
    vote(User, Array),
    getVoteAvgAux(Array, 0, Sum, 0, Num),
    Avg is Sum / Num.

getVoteAvgAux([Movie-Rating | More], AuxSum, Sum, AuxNum, Num) :-
    NewAuxSum is AuxNum + Rating,
    NewAuxNum is AuxNum + 1,
    getVoteAvgAux(More, NewAuxSum, Sum, NewAuxNum, Num).

getVoteAvgAux([], Sum, Sum, Num, Num).


% ex_3
likedBetter(User1, User2) :-
    vote(User1, Votes1),
    vote(User2, Votes2),
    checkVotes(Votes1, Votes2).

checkVotes([_-Rating | _], Votes2) :-
    checkIfHigher(Rating, Votes2), !.

checkVotes([_ | More], Votes2) :-
    checkVotes(More, Votes2).

checkIfHigher(Rating, [_-Rating2 | More]) :-
    Rating > Rating2,
    checkIfHigher(Rating, More).

checkIfHigher(_, []).


% ex_4
recommends(User, Movie) :-
    vote(User, Votes), !,
    vote(AnotherUser, AnotherVotes),
    User \== AnotherUser,
    checkSameMovies(Votes, AnotherVotes),
    getNewMovie(Votes, AnotherVotes, Movie), !.


checkSameMovies([], _).
checkSameMovies([Movie-_ | More], AnotherVotes) :-
    member(Movie-R, AnotherVotes),
    checkSameMovies(More, AnotherVotes).


getNewMovie(Votes, [Movie-_ | _], Movie) :-
    (\+ member(Movie-R, Votes)), !.

getNewMovie(Votes, [_ | More], Movie) :-
    getNewMovie(Votes, More, Movie).


% ex_5
invert(PredicateSymbol, Arity) :-
    retractAll(PredicateSymbol, Arity, Array),
    assertAll(Array).

retractAll(PredicateSymbol, Arity, [Predicate | Array]) :-
    length(Args, Arity),
    Predicate =.. [PredicateSymbol | Args],
    retract(Predicate),
    retractAll(PredicateSymbol, Arity, Array).

retractAll(_, _, []).

assertAll([]).
assertAll([Predicate | Rest]) :-
    asserta(Predicate),
    assertAll(Rest).


% ex_6
onlyOne(User1, User2, OnlyOneList) :-
    vote(User1, Films1),
    vote(User2, Films2),
    getExclusives(Films1, Films2, OnlyOneListAux1),
    getExclusives(Films2, Films1, OnlyOneListAux2),
    append(OnlyOneListAux1, OnlyOneListAux2, OnlyOneList).


getExclusives([], _, []).

getExclusives([Movie-_ | Rest], OtherList, [Movie | FinalRest]) :-
    (\+ member(Movie-R, OtherList)), !,
    getExclusives(Rest, OtherList, FinalRest).

getExclusives([_ | Rest], OtherList, FinalRest) :-
    getExclusives(Rest, OtherList, FinalRest).


% ex_7
filmVoting :-
    film(Name, _, _, _),
    getUsersArray(Name, [], Array),
    assert(filmUsersVotes(Name, Array)),
    fail.

filmVoting.


getUsersArray(Name, AuxArray, Array) :-
    vote(UserName, VotesArray),
    member(Name-Rating, VotesArray),
    (\+ member(UserName-_, AuxArray)), !,
    getUsersArray(Name, [UserName-Rating | AuxArray], Array).

getUsersArray(_, Array, Array).


% ex_8
dumpDataBase(FileName) :-
    tell(FileName),
    getUser,
    getFilm,
    getVote,
    told.

getUser :-
    user(Name, Year, Country),
    Predicate =.. [user, Name, Year, Country],
    write(Predicate), nl,
    fail.

getUser.

getFilm :-
    film(Name, Cats, Time, Rating),
    Predicate =.. [film, Name, Cats, Time, Rating],
    write(Predicate), nl,
    fail.

getFilm.

getVote :-
    vote(Name, Votes),
    Predicate =.. [vote, Name, Votes],
    write(Predicate), nl,
    fail.

getVote.