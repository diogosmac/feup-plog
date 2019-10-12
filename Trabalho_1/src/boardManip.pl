% introduz uma determinada peça numa determinada posiçao do tabuleiro
% Linha - Linha que queremos (a comecar em 1)
% Coluna - Coluna que queremos (a comecar em 1)
% TabIn - Tabuleiro input, antes de a peça ser jogada
% TabOut - Tabuleiro output, retornado depois de a peça ter sido colocada na posiçao

% ATENCAO: TALVEZ QUEIRA ISTO AO CONTRARIO (ATLINHA A ACABAR EM 7 EM VEZ DE 1)

jogaPeca(Linha, Coluna, Peca, TabIn, TabOut) :-
    atualizaLinha(Linha, Coluna, Peca, TabIn, TabOut).

atualizaLinha(1, Coluna, Peca, [Linha | Mais], [NovaLinha | Mais]) :-
    atualizaColuna(Coluna, Peca, Linha, NovaLinha).

atualizaLinha(N, Coluna, Peca, [Linha | Mais], [Linha | MaisLinhas]) :-
    N > 1,
    Next is N-1,
    atualizaLinha(Next, Coluna, Peca, Mais, MaisLinhas).

atualizaColuna(1, Peca, [_ | Resto], [Peca | Resto]).
atualizaColuna(N, Peca, [P | Resto], [P | Mais]) :-
    N > 1,
    Next is N - 1,
    atualizaColuna(Next, Peca, Resto, Mais).

% ---------------------------------------------------------------------

% retorna atraves de Peca o que se encontra numa determinada posicao do tabuleiro
% Linha - Linha que queremos (a comecar em 1)
% Coluna - Coluna que queremos (a comecar em 1)
% Tab - Estado do tabuleiro atual
% Peca - Peca que e retonada pelo predicado

% ATENCAO: TALVEZ QUEIRA ISTO AO CONTRARIO (ANLINHA A ACABAR EM 7 EM VEZ DE 1)

analisaPecaEmPosicao(Linha, Coluna, Tab, Peca) :-
    analisaLinha(Linha, Coluna, Tab, Peca).

analisaLinha(1, Coluna, [Linha | Mais], Peca) :-
    analisaColuna(Coluna, Linha, Peca).

analisaLinha(N, Coluna, [Linha | Mais], Peca) :-
    N > 1,
    Next is N-1,
    analisaLinha(Next, Coluna, Mais, Peca).

analisaColuna(1, [Peca | Resto], Peca).

analisaColuna(N, [P | Resto], Peca) :-
    N > 1,
    Next is N - 1,
    analisaColuna(Next, Resto, Peca).
    