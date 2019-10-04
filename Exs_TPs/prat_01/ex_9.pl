aluno(joao, paradigmas).
aluno(maria, paradigmas).
aluno(joel, lab2).
aluno(joel, estruturas).
frequenta(joao, feup).
frequenta(maria, feup).
frequenta(joel, ist).
professor(carlos, paradigmas).
professor(ana_paula, estruturas).
professor(pedro, lab2).
funcionario(pedro, ist).
funcionario(ana_paula, feup).
funcionario(carlos, feup). 

daAulas(Prof, Aluno):-
    professor(Prof, UC), aluno(Aluno, UC), funcionario(Prof, Fac), frequenta(Aluno, Fac).

fazParte(Pessoa, Fac):-
    funcionario(Pessoa, Fac).

fazParte(Pessoa, Fac):-
    frequenta(Pessoa, Fac).