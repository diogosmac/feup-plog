:- dynamic player/3, game/3, played/4.
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
playedALot(Player) :-
    played(Player, _, HoursPlayed, _),
    HoursPlayed >= 50.

% ex_2
isAgeAppropriate(Name, Game) :-
    player(Name, _, Age),
    game(Game, _, GameAge),
    Age >= GameAge.

% ex_3
updatePlayer(Player, Game, Hours, Percentage) :-
    retract(played(Player, Game, NumHours, CurPercentage)),
    NewHours is NumHours + Hours,
    AuxPercentage is CurPercentage + Percentage,
    (AuxPercentage > 100 -> NewPercentage = 100;
    NewPercentage = AuxPercentage),
    assert(played(Player, Game, NewHours, NewPercentage)).

% ex_4
listGamesOfCategory(Cat) :-
    game(Name, Cats, Age),
    checkGameCat(Cat, Cats),
    write(Name), write(' ('),
    write(Age), write(')'), nl,
    fail.

listGamesOfCategory(_).

checkGameCat(Cat, [CurCat | _]) :-
    Cat == CurCat.

checkGameCat(Cat, [CurCat | Rest]) :-
    Cat \== CurCat,
    checkGameCat(Cat, Rest).


% ex_5
timePlayingGames(Player, Games, ListOfTimes, SumTimes) :-
    timePlayingAux(Player, Games, ListOfTimes, 0, SumTimes).


timePlayingAux(_, [], [0], 0, 0) :- !.
timePlayingAux(_, [], [], SumTimes, SumTimes) :- !.

timePlayingAux(Player, [Game | Rest], [Time | ListOfTimes], AuxSum, SumTimes) :-
    played(Player, Game, Time, _), !,
    NewAuxSum is AuxSum + Time,
    timePlayingAux(Player, Rest, ListOfTimes, NewAuxSum, SumTimes).

timePlayingAux(Player, [_ | Rest], ListOfTimes, AuxSum, SumTimes) :-
    timePlayingAux(Player, Rest, ListOfTimes, AuxSum, SumTimes).


% ex_6
fewHours(Player, Games) :-
    fewHoursAux(Player, [], Games).

fewHoursAux(Player, AuxGames, Games) :-
    played(Player, Game, Hours, _),
    Hours < 10,
    (\+ member(Game, AuxGames)), !,
    fewHoursAux(Player, [Game | AuxGames], Games).

fewHoursAux(_, Games, Games).


% ex_7
ageRange(MinAge, MaxAge, Players) :-
    findall(Player, (player(Player, _, Age), Age >= MinAge, Age =< MaxAge), Players).


% ex_8
averageAge(Game, AverageAge) :-
    findall(Age, (played(Player, Game, _, _), player(_, Player, Age)), AllAges),
    sumlist(AllAges, AgeSum),
    length(AllAges, Length),
    Length > 0,
    AverageAge is AgeSum / Length.


% ex_9
mostEffectivePlayers(Game, Players) :-
    findall(Ratio, (played(Player, Game, HoursPlayed, PercentUnlocked), Ratio is
    PercentUnlocked / HoursPlayed) , List),
    max_member(Max, List),
    findall(Player2, (played(Player2, Game, HoursPlayed2, PercentUnlocked2), Ratio2 is
    PercentUnlocked2 / HoursPlayed2, Ratio2 == Max) , Players).


% ex_10
checkAgeGames(X):-
    player(Y, X, Z), !,
    \+ ( played(X, G, L, M),
    game(G, N, W),
    W > Z ).
% o predicado verifica se o jogador com username X joga algum jogo cuja idade minima
% e superior a sua idade.
% o cut do programa trata-se de um cut verde, uma vez que nao influencia as respostas dadas pelo programa,
% mas evita a ocorrencia de verificacoes inuteis quando se da backtracking, de modo
% a "podar" a arvore de pesquisa.
% o predicado poderia-se chamar, por exemplo, checkAgeGames.


% ex_11
% consideraria uma lista de listas, em que, no caso da matriz 5x5 do enunciado,
% a primeira sub-lista conteria 4 elementos, representando a distancia ate aos
% outros elementos; a segunda sub-lista conteria 3 elementos, etc.
% basicamente guardar apenas metade da matriz, de modo a evitar redundancia,
% e tirando partido da simetria da mesma.
% [[8, 2, 7, 7], [2, 6, 4], [6, 3], [6]].

% ex_12
estao_longe(Dist, MatDist, Pares) :-
    setof(First/Second, getDist(First, Second, MatDist, Dist), Pares).


getDist(First, Second, MatDist, Distance) :-
    nth1(First, MatDist, Line),
    nth1(AuxSecond, Line, Element),
    Second is AuxSecond + First,
    Element >= Distance.

% ex_13
% esta estrutura podera ser representada atraves de uma especie de arvore binaria.
% o programa poderia conter uma base de factos, que representam os nos intermedios,
% sendo que cada um deles era representado por um predicado com 3 argumentos: 
% o id do no, o id/nome do no/objeto descendente da sua esquerda, e o mesmo para a sua
% direita.
no(1, 2, brazil).
no(2, 3, 9).
no(3, 4, 8).
no(4, 5, reino_unido).
no(5, australia, 6).
no(6, 7, georgia_do_sul).
no(7, sta_helena, anguila).
no(8, servia, franca).
no(9, 10, irlanda).
no(10, niger, india).


% ex_14
distance(Flag1, Flag2, Distance) :-
    distanceAux(Flag1, Flag2, 0, Distance).

distanceAux(Id1, Id2, AuxDistance, Distance) :-
    getNo(NewId1, Id1),
    getNo(NewId2, Id2), !,
    NewAuxDistance is AuxDistance + 1,
    checkDistance(NewId1, NewId2, NewAuxDistance, Distance).

checkDistance(Id, Id, Distance, Distance) :- !.
checkDistance(Id1, Id2, AuxDistance, Distance) :-
    distanceAux(Id1, Id2, AuxDistance, Distance).

getNo(Id, Flag) :-
    no(Id, _, Flag);
    no(Id, Flag, _);
    (integer(Flag), Id = Flag).
