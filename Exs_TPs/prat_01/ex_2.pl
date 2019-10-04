pilot('Lamb').
pilot('Besenyei').
pilot('Chambliss').
pilot('MacLean').
pilot('Mangold').
pilot('Jones').
pilot('Bonhomme').

team('Breitling', 'Lamb').
team('Red Bull', 'Besenyei').
team('Red Bull', 'Chambliss').
team('Mediterranean Racing Team', 'MacLean').
team('Cobra', 'Mangold').
team('Matador', 'Jones').
team('Matador', 'Bonhomme').

hasPlane('MX2', 'Lamb').
hasPlane('Edge540', 'Besenyei').
hasPlane('Edge540', 'Chambliss').
hasPlane('Edge540', 'MacLean').
hasPlane('Edge540', 'Mangold').
hasPlane('Edge540', 'Jones').
hasPlane('Edge540', 'Bonhomme').

circuit('Instanbul').
circuit('Budapest').
circuit('Porto').

winner('Porto', 'Jones').
winner('Budapest', 'Mangold').
winner('Instanbul', 'Mangold').

numGates('Instanbul', 9).
numGates('Budapest', 6).
numGates('Porto', 5).

winnerTeam(Race, Team):-
    winner(Race, Pilot), team(Team, Pilot).


# para a ultima pergunta:
# hasPlane(Plane, Pilot), Plane \= 'Edge540'. 