:- module(queens,[
    prolog_queens/2,
    clpfd_queens/2
    ]).
:- use_module(library(clpfd)).

%%	prolog_queens(+N, -Queens) is nondet.
%
%	@param	Queens is a list of column numbers for placing the queens.
%	@author Richard A. O'Keefe (The Craft of Prolog)

prolog_queens(N, Queens) :-
	length(Queens, N),
	board(Queens, Board, 0, N, _, _),
	queens(Board, 0, Queens).

board([], [], N, N, _, _).
board([_|Queens], [Col-Vars|Board], Col0, N, [_|VR], VC) :-
	Col is Col0+1,
	functor(Vars, f, N),
	constraints(N, Vars, VR, VC),
	board(Queens, Board, Col, N, VR, [_|VC]).

constraints(0, _, _, _) :- !.
constraints(N, Row, [R|Rs], [C|Cs]) :-
	arg(N, Row, R-C),
	M is N-1,
	constraints(M, Row, Rs, Cs).

queens([], _, []).
queens([C|Cs], Row0, [Col|Solution]) :-
	Row is Row0+1,
	select(Col-Vars, [C|Cs], Board),
	arg(Row, Vars, Row-Row),
	queens(Board, Row, Solution).
	
	

%%	clpfd_queens(?N, ?Cols) is nondet.
%
%	@param The n-th element of Cols tells the row in which the
%	queen in the n-th column is placed.
%	@author Markus Triska

clpfd_queens(N, Cols) :-
	queens(N, Cols),
	labeling([ff], Cols).

queens(N, Qs) :-
	length(Qs, N),
	Qs ins 1..N,
	safe_queens(Qs).

safe_queens([]).
safe_queens([Q|Qs]) :-
	safe_queens(Qs, Q, 1),
	safe_queens(Qs).

safe_queens([], _, _).
safe_queens([Q|Qs], Q0, D0) :-
	Q0 #\= Q,
	abs(Q0 - Q) #\= D0,
	D1 #= D0 + 1,
	safe_queens(Qs, Q0, D1).