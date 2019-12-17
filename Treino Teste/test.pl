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


madeItThrough(Id) :-
    performance(Id, List),
    member(120, List).


juriTimes(Participants, JuriMember, Times, Total) :-
    juriTimesAux(Participants, JuriMember, Times, 0, Total).


juriTimesAux([Participant | Rest], JuriMember, [Time | Times], AuxTotal, Total) :-
    performance(Participant, JuriList),
    nth1(JuriMember, JuriList, Time),
    NewAuxTotal is AuxTotal + Time,
    juriTimesAux(Rest, JuriMember, Times, NewAuxTotal, Total).

juriTimesAux([], _, [], Total, Total).


patientJuri(JuriMember) :-
    performance(Participant1, JuriList1),
    nth1(JuriMember, JuriList1, 120),
    performance(Participant2, JuriList2),
    Participant1 \= Participant2,
    nth1(JuriMember, JuriList2, 120).


bestParticipant(P1, P2, P) :-
    performance(P1, List1),
    performance(P2, List2),
    sumlist(List1, Sum1),
    sumlist(List2, Sum2),
    chooseBestParticipant(P1, P2, Sum1, Sum2, P).

chooseBestParticipant(P1, _, Sum1, Sum2, P1) :-
    Sum1 > Sum2.

chooseBestParticipant(_, P2, Sum1, Sum2, P2) :-
    Sum2 > Sum1.


allPerfs :-
    performance(Id, ListTimes),
    participant(Id, _, Performance),
    write(Id), write(':'),
    write(Performance), write(':'),
    write(ListTimes), nl,
    fail.

allPerfs.

juriFans(FansList) :-
    findall(Participant-List, getFans(Participant, List), FansList).

getFans(Participant, List) :-
    performance(Participant, JuriList),
    length(JuriList, N),
    getFansAux(JuriList, 1, N, List).

getFansAux(JuriList, CurNumber, N, FinalList) :-
    CurNumber =< N, !,
    nth1(CurNumber, JuriList, JuriTime),
    checkJuri(JuriList, CurNumber, JuriTime, N, FinalList).

getFansAux(_, _, _, []).

checkJuri(JuriList, CurNumber, JuriTime, N, [CurNumber | FinalList]) :-
    JuriTime =:= 120, !,
    NewCurNumber is CurNumber + 1,
    getFansAux(JuriList, NewCurNumber, N, FinalList).

checkJuri(JuriList, CurNumber, _, N, FinalList) :-
    NewCurNumber is CurNumber + 1,
    getFansAux(JuriList, NewCurNumber, N, FinalList).



eligibleOutcome(Id,Perf,TT) :-
    performance(Id,Times),
    madeItThrough(Id),
    participant(Id,_,Perf),
    sumlist(Times,TT).


nextPhase(N, P) :-
    setof(TT-Id-Perf, eligibleOutcome(Id, Perf, TT), TotalList),
    reverse(TotalList, TotalReversedList),
    selectNElements(TotalReversedList, N, P).

selectNElements([Element | Rest], N, [Element | RestP]) :-
    N > 0,
    NewN is N - 1,
    selectNElements(Rest, NewN, RestP).

selectNElements(_, 0, []).



% --------------------

:- dynamic user/3, film/4, vote/2, raro/1.
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

raro(Movie) :-
    film(Movie, _, Time, _),
    checkNotNormal(Time).

checkNotNormal(Time) :-
    Time < 60.

checkNotNormal(Time) :-
    Time > 120.


happierGuy(User1, User2, HappierGuy) :-
    vote(User1, List1),
    vote(User2, List2),
    getSoma(List1, 0, Soma1),
    getSoma(List2, 0, Soma2),
    length(List1, Len1),
    length(List2, Len2),
    Media1 is Soma1 / Len1,
    Media2 is Soma2 / Len2,
    checkHappier(User1, User2, Media1, Media2, HappierGuy).

getSoma([_-Rating | Rest], AuxSoma, Soma) :-
    NewAuxSoma is AuxSoma + Rating,
    getSoma(Rest, NewAuxSoma, Soma).

getSoma([], Soma, Soma).

checkHappier(User1, _, Media1, Media2, User1) :-
    Media1 > Media2.

checkHappier(_, User2, Media1, Media2, User2) :-
    Media2 > Media1.




likedBetter(User1, User2) :-
    vote(User1, List1),
    vote(User2, List2),
    getHigher(List1, 0, H1),
    getHigher(List2, 0, H2),
    H1 > H2.

getHigher([_-Rating | Rest], AuxMax, Max) :-
    Rating > AuxMax, !,
    NewAuxMax is Rating,
    getHigher(Rest, NewAuxMax, Max).

getHigher([_ | Rest], AuxMax, Max) :-
    getHigher(Rest, AuxMax, Max).

getHigher([], Max, Max).






recommends(User, Movie) :-
    vote(User, MoviesHeSaw), !,
    vote(OtherUser, MoviesOtherSaw),
    User \= OtherUser, !,
    checkCoincidentMovies(MoviesHeSaw, MoviesOtherSaw, [Movie-_ | Rest]).

checkCoincidentMovies([MovieHeSaw-_ | Rest], MoviesOtherSaw, Remainder) :-
    select(MovieHeSaw-R, MoviesOtherSaw, NewMoviesOtherSaw),
    checkCoincidentMovies(Rest, NewMoviesOtherSaw, Remainder).

checkCoincidentMovies([], [RestMovies], [RestMovies]).


invert(PredSymbol, Arity) :-
    retractAll(PredSymbol, Arity, List),
    assertAll(List).

retractAll(PredSymbol, Arity, [G | List]) :-
    length(Args, Arity),
    G =.. [PredSymbol | Args],
    retract(G), !,
    retractAll(PredSymbol, Arity, List).

retractAll(_, _, []).

assertAll([G | Rest]) :-
    asserta(G),
    assertAll(Rest).

assertAll([]).




onlyOne(User1, User2, OnlyOneList) :-
    vote(User1, Movies1),
    vote(User2, Movies2),
    getOnlyLeft(Movies1, Movies2, Result1),
    getOnlyLeft(Movies2, Movies1, Result2),
    append(Result1, Result2, OnlyOneList).

getOnlyLeft([Movie-_ | Rest], Movies2, [Movie | Result]) :-
    (\+ member(Movie-R, Movies2)), !,
    getOnlyLeft(Rest, Movies2, Result).

getOnlyLeft([_ | Rest], Movies2, Result) :-
    getOnlyLeft(Rest, Movies2, Result).

getOnlyLeft([], _, []).



filmVoting :-
    findall(Movie, film(Movie, _, _, _), ListMovies),
    generateEntries(ListMovies).


generateEntries([Movie | Rest]) :-
    findall(User-Rating, (vote(User, Movies), member(Movie-Rating, Movies)), ListOfRatings),
    assert(filmUsersVotes(Movie, ListOfRatings)),
    generateEntries(Rest).

generateEntries([]).



dumpDataBase(Filename) :-
    tell(Filename),
    listing(user),
    listing(film),
    listing(vote),
    told.