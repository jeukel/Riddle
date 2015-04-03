rotarMatriz([],[],[]).
rotarMatriz(L1,L1,L1).
rotarMatriz([],L1,L1).
rotarMatriz([M|C], Temp , Res):- tomarLista(M , Temp , [], A),invertir(A , [], B),
	rotarMatriz(C,B,Res).

tomarLista([],[],[],[]).
tomarLista([],[],L1,L1).
tomarLista([],L1,L1,L1).
tomarLista([MH|MC], [], Lista, Res):-tomarLista(MC,[], [[MH]|Lista],Res).
tomarLista([MH|MC],[Lista2|L2C],Lista, Res):- tomarLista(MC,L2C, [[MH|Lista2]|Lista],Res).

%Remplaza elimina pieza de figura original (listas)
repElem(A,[],U,Z) :- append(U,A,Z).
repElem([Lm|LmX],[Lp|LpX], U, Z) :- notImplies(Lm,Lp,D), append(U,[D],T),repElem(LmX,LpX,T,Z).

%TABLA DE VERDAD
notImplies(X1,X2,R):- (X1==x,X2==x,R = o);(X1==o,X2==o,R = o);
	 (X1==x,X2==o,R = x).
	 
elimColumn(M1,Mres):- rait(M1,[],Res), invertir(Res,[],Mres).

rait([],M,M).
rait([H|C],T,Res):-quitarCabeza(H,HRes),rait(C,[HRes|T],Res). 

invertir([],[],[]).
invertir([],L1,L1).	
invertir([L|C],Temp,Res):- invertir(C , [L|Temp], Res).

quitarCabeza([_|C],C).

insIzq(Z1, Z2, Tz) :- reverse(Z1, T), equ(T, L1), reverse(Z2, L2), append(L2, L1, D), reverse(D, R), Tz = R.
insDer(Z1, Z2, Tz) :- equ(Z2, L2), append(Z1,L2,Tz).

equ([X|Tz], Res):- X == 0 , Res = Tz. 

%Compara que los elementos de LP esten en Lm.Tienen que estar consecutivos.
compElem([],[]).
compElem(_,[]).
compElem([Lm|LmX],[Lp|LpX]) :- Lm == Lp, compElem(LmX,LpX).

%Compara todas las filas de la original contra la pieza.
compRows([],[]).
compRows(_,[]).
compRows([Lm|LmX],[Lp|LpX]) :- compElem(Lm,Lp), compRows(LmX,LpX).

%Tiene que ir moviendo la pieza hacia abajo.
compMatrix([],[],X,X).
compMatrix([Lm|LmX],[Lp|LpX],T, Y) :- (compRows([Lm|LmX],[Lp|LpX]),compMatrix([],[],T,Y));R is T+1,compMatrix(LmX,[Lp|LpX],R,Y).

compMatrix([],X,X).
compMatrix(Lm,Lp, Y):- compMatrix(Lm,Lp,1,X),compMatrix([],X,Y).

compPiece([],_,X,Y,X,Y).
compPiece(_,[],X,Y,X,Y).
compPiece([],[],X,Y,X,Y).
compPiece([Lm|LmX],[Lp|LpX],T1,T2,X,Y) :-
	(compMatrix([Lm|LmX],[Lp|LpX],T3),compPiece([],[Lp|LpX],T1,T3,X,Y));
	(elimColumn([Lm|LmX],Mres),Xt is T1 + 1,compPiece(Mres,[Lp|LpX],Xt,T2,X,Y)).

%-----------------------------------------------------------------------------

%Function for getting props of pieces
getProps([Name|List],Z) :- append([],[Name],A), append([],List,B), [A|[B]] = Z.
getPropsRec([[Name|List]|Xs],[Z|[Zx|Tx]],Lf) :- append(Z,[Name],A), append(Zx,List,B), Lf = [A|[B]].

%Recursive function for getting props of pieces
pieceS([],X,X).
pieceS([Lp|LpX],U,Z) :- getPropsRec([Lp],U,Lf), pieceS(LpX,Lf,Z).

%Do
%figures([Lp|LpX], Z) :- getProps(Lp,U), pieceS(LpX,U,Z).

%--------------------------------------------------------------------------------

figures(Or, [Lp|LpX], Z) :- getProps(Lp,U), pieceS(LpX,U,Z), piecesManager(Or,B).

%Recursive function for comparing pieces
piecesManager(_,[]).
piecesManager(Or, [Pc|Pcs]) :- compPiece(Or, Pc), piecesManager(Or, Pcs).
