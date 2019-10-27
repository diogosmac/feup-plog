:- consult('ex_1.pl').

% fazer com findAll

encontraTodosCaminhos(NoInicial, NoFinal, Caminhos) :-
    findall(Solucao, pesqProf(NoInicial, NoFinal, Solucao), Caminhos).