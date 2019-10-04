comprou(joao, honda).
ano(honda, 1997). 
comprou(joao, uno).
ano(uno, 1998).
valor(honda, 20000).
valor(uno, 7000).

pode_vender(Pessoa, Carro, AnoAtual):-
    comprou(Pessoa, Carro), ano(Carro, Ano), X is AnoAtual - Ano, X < 10, 
    valor(Carro, Valor), Valor < 10000.