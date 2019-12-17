:- use_module(library(lists)).
:- use_module(library(between)).

airport('Aeroporto Francisco Sa Carneiro', 'LPPR', 'Portugal').
airport('Aeroporto Humberto Delgado', 'LPPT', 'Portugal').
airport('Aeropuerto Adolfo Suarez Madrid-Barajas', 'LEMD', 'Spain').
airport('Aeroport de Paris-Charles-de-Gaulle Roissy Airport', 'LFPG', 'France').
airport('Aeroporto Internazionale di Roma-Fiumicino - Leonardo da Vinci', 'LIRF', 'Italy').

company('TAP', 'TAP Air Portugal', 1945, 'Portugal').
company('RYR', 'Ryanair', 1984, 'Ireland').
company('AFR', 'Societe Air France, S.A.', 1933, 'France').
company('BAW', 'British Airways', 1974, 'United Kingdom').

flight('TP1923', 'LPPR', 'LPPT', 1115, 55, 'TAP').
flight('TP1968', 'LPPT', 'LPPR', 2235, 55, 'TAP').
flight('TP842', 'LPPT', 'LIRF', 1450, 195, 'TAP').
flight('TP843', 'LIRF', 'LPPT', 1935, 195, 'TAP').
flight('FR5483', 'LPPR', 'LEMD', 630, 105, 'RYR').
flight('FR5484', 'LEMD', 'LPPR', 1935, 105, 'RYR').
flight('AF1024', 'LFPG', 'LPPT', 940, 155, 'AFR').
flight('AF1025', 'LPPT', 'LFPG', 1310, 155, 'AFR'). 

% ex_1
short(Flight) :-
    flight(Flight, _, _, _, Duration, _),
    Duration < 90.

% ex_2
shorter(Flight1, Flight2, Short) :-
    flight(Flight1, _, _, _, Duration1, _),
    flight(Flight2, _, _, _, Duration2, _),
    shorterAux(Flight1, Flight2, Duration1, Duration2, Short).

shorterAux(Flight1, _, Duration1, Duration2, Flight1) :-
    Duration1 < Duration2.


shorterAux(_, Flight2, Duration1, Duration2, Flight2) :-
    Duration2 < Duration1.


% ex_3
arrivalTime(Flight, Arrival) :-
    flight(Flight, _, _, Init, Time, _),
    InitMins is Init mod 100,
    InitHours is Init // 100,
    AuxArrivalMins is InitMins + Time,
    ArrivalMins is AuxArrivalMins mod 60,
    ArrivalHours is (InitHours + (AuxArrivalMins // 60)),
    Arrival is (ArrivalHours * 100 + ArrivalMins).


% ex_4
countries(Company, ListOfCompanies) :-
    countriesAux(Company, [], ListOfCompanies).

countriesAux(Company, AuxList, ListOfCompanies) :-
    airport(_, Aeroport, Country),
    (\+ member(Country, AuxList)),
    hasFlights(Company, Aeroport), !,
    countriesAux(Company, [Country | AuxList], ListOfCompanies).

countriesAux(_, List, List).

hasFlights(Company, Aeroport) :-
    flight(_, Aeroport, _, _, _, Company), !.

hasFlights(Company, Aeroport) :-
    flight(_, _, Aeroport, _, _, Company), !.


% ex_5
pairableFlights :-
    flight(Flight2, Aeroport, _, Time2, _, _),
    flight(Flight1, _, Aeroport, _, _, _),
    arrivalTime(Flight1, Time1),
    TimeMins1 is (((Time1 // 100) * 60) + (Time1 mod 100)),
    TimeMins2 is (((Time2 // 100) * 60) + (Time2 mod 100)),
    Flight1 \= Flight2,
    Diff is TimeMins2 - TimeMins1,
    Diff =< 90,
    Diff >= 30,
    write(Aeroport), write(' - '),
    write(Flight1), write(' \\ '),
    write(Flight2), nl,
    fail.

pairableFlights.


% ex_6
tripDays(Trip, Time, FlightTimes, Days) :-
    tripDaysAux(Trip, Time, FlightTimes, 1, Days).

tripDaysAux([Country1, Country2 | Trip], Time, [TimeFlight | FlightTimes], AuxDays, Days) :-
    airport(_, Airport1, Country1),
    airport(_, Airport2, Country2),
    flight(Flight, Airport1, Airport2, TimeFlight, _, _), !,
    arrivalTime(Flight, ArrivalTime),
    checkDays(Time, TimeFlight, AuxDays, NewAuxDays),
    ArrivalTimeMins is (((ArrivalTime // 100) * 60) + (ArrivalTime mod 100)),
    NewTimeMins is ArrivalTimeMins + 30,
    NewTime is (((NewTimeMins // 60) * 100) + (NewTimeMins mod 60)),
    tripDaysAux([Country2 | Trip], NewTime, FlightTimes, NewAuxDays, Days).

tripDaysAux([FinalCountry], _, [], Days, Days).
tripDaysAux([], _, [], Days, Days).

checkDays(Time, TimeFlight, Days, Days) :-
    Time =< TimeFlight, !.

checkDays(Time, TimeFlight, AuxDays, Days) :-
    Time > TimeFlight,
    Days is AuxDays + 1.
    

% ex_7
avgFlightLengthFromAirport(Airport, Length) :-
    findall(Duration, (flight(_, Airport, _, _, Duration, _)), ListOfDurations),
    sumlist(ListOfDurations, Sum),
    length(ListOfDurations, L),
    Length is Sum / L.

% ex_8
mostInternational(List) :-
    findall(NumCountries-ListCompanies, (setof(Company, getNumCountries(Company, NumCountries), ListCompanies)), AlmostFinalList),
    reverse(AlmostFinalList, FinalList),
    returnList(FinalList, List).

getNumCountries(Company, NumCountries) :-
    company(Company, _, _, _),
    findall(Country, (flight(_, Airport, _, _, _, Company), airport(_, Airport, Country)), ListDep),
    findall(Country, (flight(_, _, Airport, _, _, Company), airport(_, Airport, Country)), ListArr),
    append(ListDep, ListArr, FullList),
    sort(FullList, FinalList),
    length(FinalList, NumCountries).

returnList([Num-List | Rest], List).
returnList([], []).

% ex_9
dif_max_2(X, Y) :- X < Y, X >= Y - 2.

make_pairs(L, PredName, [X-Y | RestOfList]) :-
    select(X, L, L1),
    select(Y, L1, L2),
    G =.. [P, X, Y], G,
    make_pairs(L2, PredName, RestOfList).


make_pairs([], _, []).


% ex_10
make_pairs_2(L, PredName, [X-Y | RestOfList]) :-
    select(X, L, L1),
    select(Y, L1, L2),
    G =.. [PredName, X, Y], G, !,
    make_pairs_2(L2, PredName, RestOfList).

make_pairs_2(L, _, []).


% ex_11
make_pairs_3(L, PredName, [X-Y | RestOfList]) :-
    select(X, L, L1),
    select(Y, L1, L2),
    G =.. [PredName, X, Y], G,
    make_pairs_3(L2, PredName, RestOfList).

make_pairs_3(L, _, []).

make_max_pairs(L, P, S) :-
    findall(Pairs, make_pairs_3(L, P, Pairs), ListOfLists),
    choose_max_pair(ListOfLists, 0, [], S).

choose_max_pair([], _, S, S).
choose_max_pair([CurrentSol | Rest], MaxLen, AuxS, S) :-
    length(CurrentSol, CurLen),
    CurLen > MaxLen, !,
    NewAuxS = CurrentSol,
    NewMaxLen = CurLen,
    choose_max_pair(Rest, NewMaxLen, NewAuxS, S).

choose_max_pair([_ | Rest], MaxLen, AuxS, S) :-
    choose_max_pair(Rest, MaxLen, AuxS, S).


% ex_12
% can win from these positions
winner(1, 1).

winningPos(X, 1).
winningPos(1, Y).
winningPos(X, X).

whitoff(Size, ListOfWinningPos) :-
    whitoffAux(Size, 1, [], [], AuxListOfWinningPos),
    sort(AuxListOfWinningPos, ListOfWinningPos).

whitoffAux(Size, Line, AuxList, AuxListOfWinningPos, ListOfWinningPos) :-
    Line =< Size,
    whitoffAuxLine(Size, Line, 1, LineList, AuxList, FinalAuxList),
    append(AuxListOfWinningPos, LineList, NewAuxListOfWinningPos),
    NewLine is Line + 1,
    whitoffAux(Size, NewLine, FinalAuxList, NewAuxListOfWinningPos, ListOfWinningPos).

whitoffAux(Size, Line, _, List, List) :-
    Line > Size.

whitoffAuxLine(Size, Line, Column, [(Line,Column) | LineList], AuxList, FinalAuxList) :-
    Column =< Size,
    whitoffVerifyPos(Line, Column, AuxList), !,
    NewColumn is Column + 1,
    whitoffAuxLine(Size, Line, NewColumn, LineList, AuxList, FinalAuxList).

whitoffAuxLine(Size, Line, Column, LineList, AuxList, FinalAuxList) :-
    Column =< Size,
    NewColumn is Column + 1,
    whitoffAuxLine(Size, Line, NewColumn, LineList, [(Line,Column) | AuxList], FinalAuxList).

whitoffAuxLine(Size, _, Column, [], List, List) :-
    Column > Size.

% if player 1 puts piece in (1, 1), wins
whitoffVerifyPos(1, 1, _).

whitoffVerifyPos(Line, Column, List) :-
    (\+ member((Line,Column), List)),
    getAllMovements(Line, Column, ListOfMoves), % generate moves for player 2
    checkIfCantWin(ListOfMoves, List), !, % player 2 shouldn't be able to win from any of these positions
    verifyPlayer1Plays(ListOfMoves, List).

verifyPlayer1Plays([], _).
verifyPlayer1Plays([(X,Y) | RestOfPlayer2Moves], List) :-
    getAllMovements(X, Y, ListOfPlayer1Pos), % possible moves for player 1
    checkPosGarantVictory(ListOfPlayer1Pos, List);
    verifyPlayer1Plays(RestOfPlayer2Moves, List).

checkPosGarantVictory([(X,Y) | Rest], List) :-
    (winner(X, Y); whitoffVerifyPos(X, Y), List), !.

checkPosGarantVictory([_ | Rest], List) :-
    checkPosGarantVictory(Rest, List).

checkIfCantWin([], _).
checkIfCantWin([(X, Y) | Rest], List) :-
    (\+ (winner(X, Y); whitoffVerifyPos(X, Y, List))),
    checkIfCantWin(Rest, List).

% gets all posible movements for a position
getAllMovements(Line, Column, ListOfMoves) :-
    setof((NewLine,NewColumn), canMoveTo(Line, Column, NewLine, NewColumn), ListOfMoves).

% can move sideways
canMoveTo(Line, Column, Line, NewColumn) :-
    C is Column - 1,
    between(1, C, NewColumn).

% can move up
canMoveTo(Line, Column, NewLine, Column) :-
    L is Line - 1,
    between(1, L, NewLine).

% can move diagonally
canMoveTo(Line, Column, NewLine, NewColumn) :-
    Line =< Column,
    L is Line - 1,
    between(1, L, Off),
    NewLine is Line - Off,
    NewColumn is Column - Off.

canMoveTo(Line, Column, NewLine, NewColumn) :-
    Line > Column,
    C is Column - 1,
    between(1, C, Off),
    NewLine is Line - Off,
    NewColumn is Column - Off.