%member(X,[X|T]).
%member(X,[H|T]) :- member(X,T).

%createRow(Z1,Z3) :- length(Z3,Z1).

%resetRow(Org, Mx, Out) :- length(Org, T), createRow(T, Z3), setZero(Z3,T).

%setZero([X|Xs],T) :- append([0], Xs, T).

%getFirst([X|Xs], T) :- X=T.

%getCols([U|Uz], W) :- length(U,W).
%printError(L1, L2) :- length(L1,X1), length(L2,X2), X1 > X2, print(ERROR).
 
equ([X|Tz], Res):- X == 0, Res = Tz.
ins(Z1, Z2, Tz) :- reverse(Z1, T), equ(T, L1), reverse(Z2, L2), append(L2, L1, D), reverse(D, R), Tz = R.

%union(Z1, Z2, Tz) :-

%what(A,B,C) :- 