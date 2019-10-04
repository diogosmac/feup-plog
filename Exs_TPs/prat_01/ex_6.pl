species(tweety, bird).
species(goldie, fish).
species(molie, worm).
species(silvester, cat).

likes(bird, worm).
likes(cat, fish).
likes(cat, bird).

friend(me, cat).
friend(A, B) :- friend(B, A).

eats(A, Animal):-
    owns(me, A), species(A, cat), likes(cat, Animal).

owns(me, silvester).

# eats(silvester, Animal).