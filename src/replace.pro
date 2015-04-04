equ([X|Tz], Res):- X == 0 , Res = Tz. 

insIzq(Z1, Z2, Tz) :- reverse(Z1, T), equ(T, L1), reverse(Z2, L2), append(L2, L1, D), reverse(D, R), Tz = R.

insDer(Z1, Z2, Tz) :- equ(Z2, L2), append(Z1,L2,Tz).

%compElem([],[]).
%compElem(_,[]).
%compElem([Lm|LmX],[Lp|LpX]) :- Lm == Lp, compElem(LmX,LpX).

compRows([],[]).
compRows(_,[]).
compRows([Lm|LmX],[Lp|LpX]) :- compElem(Lm,Lp), compRows(LmX,LpX).

compMatrix([],[]).
compMatrix([Lm|LmX],[Lp|LpX]) :- compRows([Lm|LmX],[Lp|LpX]); compMatrix(LmX,[Lp|LpX]).  

%compPiece([Lm|LmX],[Lp|LpX],Nm) :- compMatrix([Lm|LmX],[Lp|LpX]); elimColumn([Lm|LmX],R); compPiece(R,[Lp|LpX],Nm).

%-----------------------------------------------------------------------------

%Function for getting props of pieces
getProps([Name|List],Z) :- append([],[Name],A), append([],List,B), [A|[B]] = Z.
%getPropsRec([[Name|List]|Xs],[Z|[Zx|Tx]],Lf) :- append(Z,[Name],A), append(Zx,List,B), Lf = [A|[B]].
getPropsRec([[Name|List]|_],[Z|[Zx|_]],Lf) :- append(Z,[Name],A), append(Zx,List,B), Lf = [A|[B]].

%Recursive function for getting props of pieces
pieceS([],X,X).
pieceS([Lp|LpX],U,Z) :- getPropsRec([Lp],U,Lf), pieceS(LpX,Lf,Z).

%Do
%figures([Lp|LpX], Z) :- getProps(Lp,U), pieceS(LpX,U,Z).

%--------------------------------------------------------------------------------

figures(Or, [Lp|LpX], Z) :- getProps(Lp,U), pieceS(LpX,U,Z), piecesManager(Or,Z).

%Recursive function for comparing pieces
piecesManager(_,[]).
piecesManager(Or, [Pc|Pcs]) :- compPiece(Or, Pc), piecesManager(Or, Pcs).

%--------------------------------------------------------------------------------

%Remplaza elimina pieza de figura original (listas)
repElem(A,[],U,Z) :- append(U,A,Z).
repElem([Lm|LmX],[Lp|LpX], U, Z) :- notImplies(Lm,Lp,D), append(U,[D],T),repElem(LmX,LpX,T,Z).

notImplies(X1,X2,R):- (X1==x,X2==x,R = o);(X1==o,X2==o,R = o);
	 (X1==x,X2==o,R = x).	 
	 
%--------------------------------------------------------------------------------
matrixByRow(A,[],Z,Z).
matrixByRow([Lm|LmX],[Lp|LpX],U, Z) :- repElem(Lm,Lp,[],T), append(U,[T],W), matrixByRow(LmX,LpX,W,Z).

%Matriz, Pieza, ContadorEnX, Cabeza, Resto.
mvX(M,P,Nx,Z) :- mvColumn(M,Nx,[],[],H,R), matrixByRow(R,P,[],U), unify(H,U,[],Z).

unify([],[],Z,Z).
unify([M1|Mx],[M2|Ms],N,Z) :- append(M1,M2,N1), append(N,[N1],T), unify(Mx,Ms,T,Z).

mvColumn([],_,T1,T2,R,H) :- reverse(T1,H), reverse(T2,R).
mvColumn([Ml|Mlx],Nx,T1,T2,H,R) :- correrY(Ml,Nx,W1,W2), append([W1],T1,Z1), append([W2],T2,Z2), mvColumn(Mlx,Nx,Z1,Z2,H,R).

%------------------------------------------------------------------------------------

correrY([],Y,Y2,X,Y2):- Y\=[],Y2\=[],reverse(Y,Z),X=Z.

correrY([M|Mc],Y,Mres2,Mres):-correrY([M|Mc],Y,1,[],[],Mres2,Mres).

correrY([M|Mc],Y,YT,T,T2,Mres2,Mres):-YT<Y,Yt is YT +1, correrY(Mc,Y,Yt,[M|T],Mc,Mres2,Mres); correrY([],T,T2,Mres,Mres2).