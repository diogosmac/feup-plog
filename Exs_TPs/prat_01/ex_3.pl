isBook('Os Maias').
autor('Os Maias', 'Eca de Queiroz').
language('Os Maias', portugues).
language('Os Maias', ingles).
typeOfBook('Os Maias', romance).
typeOfBook('Os Maias', ficcao).
nacionality('Eca de Queiros', portugues).

authorType(Author, Type):-
    typeOfBook(Book, Type), autor(Book, Author).


# a) autor('Os Maias', Author).
# b) authorType(Author, romance).
# c) authorType(Author, ficcao), authorType(Author, Type), Type \= ficcao.