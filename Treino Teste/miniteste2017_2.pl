:- dynamic played/4.
:- use_module(library(lists)).

player('Danny', 'Best Player Ever', 27).
player('Annie', 'Worst Player Ever', 24).
player('Harry', 'A-Star Player', 26).
player('Manny', 'The Player', 14).
player('Jonny', 'A Player', 16).

game('5 ATG', [action, adventure, open-world, multiplayer], 18).
game('Carrier Shift: Game Over', [action, fps, multiplayer, shooter], 16).
game('Duas Botas', [action, free, strategy, moba], 12).

played('Best Player Ever', '5 ATG', 3, 83).
played('Worst Player Ever', '5 ATG', 52, 9).
played('The Player', 'Carrier Shift: Game Over', 44, 22).
played('A Player', 'Carrier Shift: Game Over', 48, 24).
played('A-Star Player', 'Duas Botas', 37, 16).
played('Best Player Ever', 'Duas Botas', 33, 22).

% ex_1
achievedLittle(Player) :-
    played(Player, _, _, Perc),
    Perc < 10.

% ex_2
isAgeAppropriate(Name, Game) :-
    player(Name, _, Age),
    game(Game, _, AgeGame), !,
    AgeGame =< Age.

% ex_3
timePlayingGames(Player, Games, ListOfTimes, SumTimes) :-
    timePlayingGames(Player, Games, ListOfTimes, 0, SumTimes).

timePlayingGames(Player, [Game | Rest], [Time | ListOfTimes], AuxTime, SumTimes) :-
    played(Player, Game, Time, _), !,
    NewAuxTime is AuxTime + Time,
    timePlayingGames(Player, Rest, ListOfTimes, NewAuxTime, SumTimes).

timePlayingGames(Player, [_ | Rest], ListOfTimes, AuxTime, SumTimes) :-
    timePlayingGames(Player, Rest, ListOfTimes, AuxTime, SumTimes).

timePlayingGames(_, [], [0], 0, 0) :- !.
timePlayingGames(_, [], [], Sum, Sum) :- !.


% ex_4
listGamesOfCategory(Cat) :-
    game(Game, Cats, Age),
    member(Cat, Cats),
    write(Game), write(' ('),
    write(Age), write(')'), nl,
    fail.

listGamesOfCategory(_).


% ex_5
updatePlayer(Player, Game, Hours, Percentage) :-
    retract(played(Player, Game, CurHours, CurPercentage)),
    NewHours is CurHours + Hours,
    NewPercentage is CurPercentage + Percentage,
    assert(played(Player, Game, NewHours, NewPercentage)).


% ex_6
littleAchievement(Player, Games) :-
    littleAux(Player, [], Games).

littleAux(Player, AuxGames, Games) :-
    played(Player, Game, _, Percentage),
    (\+ member(Game, AuxGames)),
    Percentage < 20, !,
    littleAux(Player, [Game | AuxGames], Games).

littleAux(Player, List, List).


% ex_7
ageRange(MinAge, MaxAge, Players) :-
    findall(Player, (player(Player, _, Age), MinAge =< Age, MaxAge >= Age), Players).


% ex_8
averageAge(Game, AverageAge) :-
    findall(Age, (played(Player, Game, _, _), player(_, Player, Age)), ListAges),
    sumlist(ListAges, SumAges),
    length(ListAges, NumAges),
    AverageAge is SumAges / NumAges.


% ex_9
mostEffectivePlayers(Game, Players) :-
    findall(Ratio, (played(_, Game, Time, Perc), Ratio is Perc / Time), AllRatios),
    max_member(BestRatio, AllRatios),
    findall(Player, (played(Player, Game, Time, Perc), Ratio is Perc / Time, Ratio =:= BestRatio), Players).