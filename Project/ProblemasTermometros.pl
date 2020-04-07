% Projeto LP 2017/2018
% Linguagem de Programacao SWI-Prolog
% Ricardo Grade, Nr 90774, LEIC-T

:- [puzzles].

% >>> 1 Predicado <<<

% Dado um Puzzle
% Uma Posicao
% Procura na lista de Termometros a lista (Termometro) que contem a Posicao

propaga([[H | _] | _], Posicao, Posicoes) :-
	member(Posicao, H), !,
	antecedentes(H, Posicao, Poss),
	sort(Poss, Posicoes).

propaga([[_ | L] | _], Posicao, Posicoes) :-
	propaga([L | _], Posicao, Posicoes).

% Dada uma lista (Termometro) e um elemento (Posicao) da mesma
% Unifica com as Posicoes a particao dessa lista (Termometro) da Cabeca a Posicao

antecedentes([Posicao | _], Posicao, [Posicao]) :- !.

antecedentes([H | L], Posicao, [H | Poss]) :-
	member(Posicao, L),
	antecedentes(L, Posicao, Poss).

% >>> 2 Predicado <<<

% Dada uma Possiblidade (lista de Posicoes) de preenchimento duma determinada linha
% A linha em questao
% As Ja_Preenchidas (lista de Posicoes)
% Verifica se todas as Posicoes da lista que contem linha inferior a linha em questao
% Estao ja representadas nas Ja_Preenchidas

nao_altera_linhas_anteriores([], _, _) :- !.

nao_altera_linhas_anteriores([(X, Y) | L], Lin, Ja_Preenchidas) :-
	X < Lin, !,
	member((X, Y), Ja_Preenchidas),
	nao_altera_linhas_anteriores(L, Lin, Ja_Preenchidas).

nao_altera_linhas_anteriores([_ | L], Lin, Ja_Preenchidas) :-
	nao_altera_linhas_anteriores(L, Lin, Ja_Preenchidas).

% >>> 3 Predicado <<<

% Dado um Puzzle
% As Ja_Preenchidas (lista de Posicoes)
% A Dim (dimensao do Puzzle)
% Uma Possiblidade (lista de Posicoes)

verifica_parcial(Puz, Ja_Preenchidas, Dim, Poss) :-
	union(Ja_Preenchidas, Poss, Lst),
	verifica_parcial_rec(Puz, Lst, Dim).
	
% Verifica se a Possiblidade e valida
% Verificando para cada Coluna se o maximo de elementos que pode conter nao e excedido

verifica_parcial_rec(_, _, 0) :- !.

verifica_parcial_rec(Puz, Lst, Dim) :-
	Dim > 0,
	conta_col(Lst, Dim, ActCol),
	col_index(Puz, Dim, PerCol),
	ActCol =< PerCol,
	Dim_menos is Dim - 1,
	verifica_parcial_rec(Puz, Lst, Dim_menos).

% Conta o numero de elementos duma dada coluna que estam contido na lista de Posicoes

conta_col([], _, 0) :- !.

conta_col([(_, Y) | R], Index, Val) :-
	Y == Index, !,
	conta_col(R, Index, Val_mais),
	Val is Val_mais + 1.

conta_col([_ | R], Index, Val) :-
	conta_col(R, Index, Val).

% Dado um Puzzle
% Um Index (nr da coluna)
% Passa os limites das Colunas

col_index([_, _, Col], Index, Val) :-
	procura_index(Col, Index, Val).

% Dada uma lista
% Um Index [1, Dim]
% Unifica com Val o valor da lista[Index]

procura_index([Val | _], 1, Val) :- !.

procura_index([_ | R], Index, Val) :-
	Index > 1,
	Index_menos is Index - 1,
	procura_index(R, Index_menos, Val).

% >>> 4 Predicado <<<

% Dado um Puzzle
% As Posicoes_linha (lista de Posicoes)
% O Total de elementos que a linha pode conter
% As Ja_Preenchidas (lista de Posicoes)
% Unifica com as Possibilidades_LS todas as Possiblidades de preenchimento da linha em questao
% Atendendo as Ja_Preenchidas

possibilidades_linha(Puz, [(X, Y) | R], Total, Ja_Preenchidas, Possibilidades_LS) :-
	length([(X, Y) | R], Dim),
	combinations(Total, [(X, Y) | R], Combs),
	intersection(Ja_Preenchidas, [(X, Y) | R], Filter),
	filtra_combs(Combs, Filter, Combs_I),
	possibilidades_linha_rec(Puz, Combs_I, Total, X, Dim, Ja_Preenchidas, Possibilidades_L),
	sort(Possibilidades_L, Possibilidades_LS).

% Testa todas as Combinacoes geradas
% Atendendo ao numero de elementos por linha
% Aos elementos com linha anterior
% Aos numero de elementos por coluna
% Adicionando as validas a Possiblidades_L

possibilidades_linha_rec(_, [], _, _, _, _, []) :- !.

possibilidades_linha_rec(Puz, [Comb | R], Total, Lin, Dim, Ja_Preenchidas, [Poss_S | Possibilidades_L]) :-
	gera_poss(Puz, Comb, [], Poss),
	conta_elementos_linha(Poss, Lin, Cont), Cont == Total,
	nao_altera_linhas_anteriores(Poss, Lin, Ja_Preenchidas),
	verifica_parcial(Puz, Ja_Preenchidas, Dim, Poss),
	sort(Poss, Poss_S), !,
	possibilidades_linha_rec(Puz, R, Total, Lin, Dim, Ja_Preenchidas, Possibilidades_L).

possibilidades_linha_rec(Puz, [_ | R], Total, Lin, Dim, Ja_Preenchidas, Possibilidades_L) :-
	possibilidades_linha_rec(Puz, R, Total, Lin, Dim, Ja_Preenchidas, Possibilidades_L).

% Dado um Puzzle
% Dada uma lista de Posicoes
% Vai adicionando na Poss (lista de Posicoes) as Posicoes do propaga de cada Posicao da lista

gera_poss(_, [], Poss, Poss) :- !.

gera_poss(Puz, [H | R], Poss_I, Poss) :-
	propaga(Puz, H, Posicoes),
	union(Poss_I, Posicoes, Poss_II),
	gera_poss(Puz, R, Poss_II, Poss).

% Dada uma lista de Posicoes
% Uma linha
% Unifica em Cont o numero de elementos que contem a linha

conta_elementos_linha([], _, 0) :- !.

conta_elementos_linha([(X, _) | R], Lin, Cont) :-
	X == Lin, !,
	conta_elementos_linha(R, Lin, Cont_mais),
	Cont is Cont_mais + 1.

conta_elementos_linha([_ | R], Lin, Cont) :-
	conta_elementos_linha(R, Lin, Cont).

% Dado N (numero de elementos)
% Os Casos Possiveis (lista de Posicoes)
% Unifica em Comb uma Combinacao possivel (lista de Posicoes)
% Atendendo aos Casos Possiveis e ao numero de elementos nela contidos

combination(0, _, []) :- !.

combination(N, Casos_P, [Ele | Comb]) :-
	N > 0,
	N_menos is N - 1,
	procura_ele(Ele, Casos_P, Casos_P_I),
	combination(N_menos, Casos_P_I, Comb).

procura_ele(Ele, [Ele | Casos_P], Casos_P).

procura_ele(Ele, [_ | Casos_P], Casos_P_I) :-
	procura_ele(Ele, Casos_P, Casos_P_I).

% Unifica com Combs todas as Combinacoes Possiveis (lista de listas de Posicoes)

combinations(N, Casos_P, Combs) :-
	findall(Comb, combination(N, Casos_P, Comb), Combs).

% Dado as Combs
% O Filter (lista de Posicoes) que contem as Posicoes de determinada linha Ja_Preenchidas
% Unifica com Combs todas as Combinacoes que contem os elementos do Filter

filtra_combs([], _, []) :- !.

filtra_combs([H | R], Filter, [H | Combs]) :-
	intersection(H, Filter, Lst_Cmp),
	Lst_Cmp == Filter, !,
	filtra_combs(R, Filter, Combs).

filtra_combs([_ | R], Filter, Combs) :-
	filtra_combs(R, Filter, Combs).

% >>> 5 Predicado <<<

% Dado o Puzzle
% Unifica com a Solucao_S uma proposta de resolucao do Puzzle

resolve([Termometros, Linhas, Colunas], Solucao_S) :-
	length(Linhas, Dim),
	resolve_rec([Termometros, Linhas, Colunas], 1, Dim, [], Solucao),
	sort(Solucao, Solucao_S).

% Testa cada Possiblidade de preenchimento de determinada linha
% Incrementando a linha a testar
% Ou retrocedendo caso se chegue a uma linha cuja lista de Possiblidades e vazia
% Ate conseguir chegar a ultima linha do Puzzle
% Ate que unifica com a Solucao as Ja_Preenchidas

resolve_rec(_, Lin, Dim, Solucao, Solucao) :-
	Lin > Dim, !.

resolve_rec(Puz, Lin, Dim, Ja_Preenchidas, Solucao) :-
	Lin >= 1, Lin =< Dim,
	gera_posicoes_linha(Lin, 1, Dim, Posicoes_linha),
	lin_index(Puz, Lin, Total),
	possibilidades_linha(Puz, Posicoes_linha, Total, Ja_Preenchidas, Possiblidades_L),
	member(Poss, Possiblidades_L),
	union(Ja_Preenchidas, Poss, Ja_Preenchidas_I),
	Lin_mais is Lin + 1,
	resolve_rec(Puz, Lin_mais, Dim, Ja_Preenchidas_I, Solucao).

% Dado um Puzzle
% Um Index (nr da linha)
% Passa os limites das Linhas

lin_index([_, Lin, _], Index, Val) :-
	procura_index(Lin, Index, Val).

% Dada um Linha
% Um Cont que comeca a 1
% A Dim (dimensao do Puzzle)
% Unifica com a Linha_Lst a lista de todas as Posicoes da linha

gera_posicoes_linha(_, Cont, Dim, []) :-
	Cont > Dim, !.

gera_posicoes_linha(Lin, Cont, Dim, [(Lin, Cont) | Linha_Lst]) :-
	Cont >= 1, Cont =< Dim,
	Cont_mais is Cont + 1,
	gera_posicoes_linha(Lin, Cont_mais, Dim, Linha_Lst).
