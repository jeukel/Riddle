equ([X|Tz], Res):- X == 0 , Res = Tz. 

insIzq(Z1, Z2, Tz) :- reverse(Z1, T), equ(T, L1), reverse(Z2, L2), append(L2, L1, D), reverse(D, R), Tz = R.

insDer(Z1, Z2, Tz) :- equ(Z2, L2), append(Z1,L2,Tz).

%coplings([A|Ax],[B|Bx],C) :- compose1(B,Bx,T), member(T,A), compose2(B,Bx,U), member(U,Ax), C=(1,2,3,4).

compElem([],[]).
compElem(_,[]).
compElem([Lm|LmX],[Lp|LpX]) :- Lm == Lp, compElem(LmX,LpX).

compRows([],[]).
compRows(_,[]).
compRows([Lm|LmX],[Lp|LpX]) :- compElem(Lm,Lp), compRows(LmX,LpX).

compMatrix([],[]).
compMatrix([Lm|LmX],[Lp|LpX]) :- compRows([Lm|LmX],[Lp|LpX]); compMatrix(LmX,[Lp|LpX]).  

sumarMatricesIzq([], M, M).
sumarMatricesIzq(M, [], M).
sumarMatricesIzq([], [], []).
sumarMatricesIzq([M1|M1C], [M2|M2C], MR):-Temp = [], append(M1, M2 , T).

compPiece([Lm|LmX],[Lp|LpX],Nm) :- compMatrix([Lm|LmX],[Lp|LpX]); elimColumn([Lm|LmX],R); compPiece(R,[Lp|LpX],Nm).