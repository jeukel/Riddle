equ([X|Tz], Res):- X == 0 , Res = Tz. 

insIzq(Z1, Z2, Tz) :- reverse(Z1, T), equ(T, L1), reverse(Z2, L2), append(L2, L1, D), reverse(D, R), Tz = R.

insDer(Z1, Z2, Tz) :- equ(Z2, L2), append(Z1,L2,Tz).  

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

rotarMatriz([],L1,L1).
rotarMatriz([M|C], Temp , Res):- not( is_list(M)),
				 tomarLista([M|C],
				 Temp,[],A),
				 reverse(A,B),
				 rotarMatriz([],B,Res).
				 
rotarMatriz([M|C], Temp , Res):- is_list(M),
				 tomarLista(M , Temp , [], A),
				 reverse(A,B),
				 rotarMatriz(C,B,Res).
				 
tomarLista([],[],L1,L1).
tomarLista([MH|MC], [], Lista, Res):-tomarLista(MC,[], [[MH]|Lista],Res).
tomarLista([MH|MC],[Lista2|L2C],Lista, Res):- tomarLista(MC,L2C, [[MH|Lista2]|Lista],Res).

norepeat([]).
norepeat([Lp|LpX]) :- norepeat_aux(Lp,LpX),repeatWrot(Lp,LpX,1), norepeat(LpX).

%TO DO
%Revizar condición de salida.
repeatWrot(_,_,N) :- N >= 4.
repeatWrot(H,LpX,N) :- rotarMatriz(H,[],Z), T is N+1, print(T),nl, norepeat_aux(Z,LpX), repeatWrot(Z,LpX,T).

norepeat_aux(_,[]).
norepeat_aux(H,[R|Rx]) :- H\=R, norepeat_aux(H,Rx).

%figures(Or,[Lp|LpX],Z) :- getProps(Lp,U), pieceS(LpX,U,Z),Z=[_|[Wtf|_]], norepeat(Wtf).
figures([Lp|LpX],Z) :- getProps(Lp,U), pieceS(LpX,U,Z),Z=[_|[Wtf|_]],reverse(Wtf,Ftw),norepeat(Ftw).

%--------------------------------------------------------------------------------

%Remplaza elimina pieza de figura original (listas)
repElem(A,[],U,Z) :- append(U,A,Z).
repElem([Lm|LmX],[Lp|LpX], U, Z) :- notImplies(Lm,Lp,D), append(U,[D],T),repElem(LmX,LpX,T,Z).

notImplies(X1,X2,R):- (X1==x,X2==x,R = o);(X1==o,X2==o,R = o);
	 (X1==x,X2==o,R = x).	 
	 
%--------------------------------------------------------------------------------
matrixByRow(_,[],Z,Z).
matrixByRow([Lm|LmX],[Lp|LpX],U, Z) :- repElem(Lm,Lp,[],T), append(U,[T],W), matrixByRow(LmX,LpX,W,Z).

%Matriz, Pieza, ContadorEnX, Resto.
mvX(M,P,Nx,Z) :- mvColumn(M,Nx,[],[],H,R), matrixByRow(R,P,[],U), unify(H,U,[],Z).

unify([],[],Z,Z).
unify([M1|Mx],[M2|Ms],N,Z) :- append(M1,M2,N1), append(N,[N1],T), unify(Mx,Ms,T,Z).

mvColumn([],_,T1,T2,R,H) :- reverse(T1,H), reverse(T2,R).
mvColumn([Ml|Mlx],Nx,T1,T2,H,R) :- correrY(Ml,Nx,W1,W2), append([W1],T1,Z1), append([W2],T2,Z2), mvColumn(Mlx,Nx,Z1,Z2,H,R).

%------------------------------------------------------------------------------------

correrY([],Y,Y2,X,Y2):- Y\=[],Y2\=[],reverse(Y,Z),X=Z.
correrY([M|Mc],Y,Mres2,Mres):-correrY([M|Mc],Y,1,[],[],Mres2,Mres).
correrY([M|Mc],Y,YT,T,T2,Mres2,Mres):-YT<Y,Yt is YT +1, correrY(Mc,Y,Yt,[M|T],Mc,Mres2,Mres); correrY([],T,T2,Mres,Mres2).

%------------------------------------------------------------------------------------

%Turns num rotations into the name of the piece.
setRotNames(_,[],Z,Z).
setRotNames(Ns,[Num|R],A,Z) :-nth1(Num,Ns,Name), append(A,[Name],U),setRotNames(Ns,R,U,Z).

%list of counted elements
getQuant([Name|Names],Rots,A,S) :- setRotNames([Name|Names],Rots,[],Z), occurrences(Z,Name,U), append(A,[U],T), getQuant_aux(Names,Z,T,S).
getQuant_aux([],_,S,S).
getQuant_aux([Name|Names],Rots,A,S) :- occurrences(Rots,Name,U), append(A,[U],T), getQuant_aux(Names,Rots,T,S).

occurrences([],_,0).
occurrences([X|Y],X,N):- occurrences(Y,X,W),N is W + 1.
occurrences([X|Y],Z,N):- occurrences(Y,Z,N),X\=Z.

%------------------------------------------------------------------------------------

%solFormat([Name|Names],Rots,A,B) :- getQuant([Name|Names],Rots,[],W), W=[U|Us], getRot(U,T), G=(Name,T), append(A,[G],Z), solFormat_aux(Names,Us,Z,B).
%solFormat_aux([Name|Names],[R|Rl],A,B) :- getRot(R,T), G=(Name,T), append(A,[G],Z), solFormat_aux(Names,Rl,Z,B).
%solFormat_aux([],[],B,B).

solFormat([Name|Names],[R|Rx],A,B) :- getRot(R,T), G=(Name,T), append(A,[G],Z), solFormat(Names,Rx,Z,B).
solFormat([],[],B,B).

getRot(A,B) :- A>=4 , T is A-4, getRot(T,B).
getRot(0,0).
getRot(1,90).
getRot(2,180).
getRot(3,270).

%------------------------------------------------------------------------------------

piecesManager(Or,[Lp|LpX],U,V,Cd,Rt) :- 
	compPiece(Or,Lp,1,1,1,R,X,Y),
	toZERO(Or,Lp,X,Y,M),	
	append(U,[X|Y],Us),
	append(V,[R],Vs),
	piecesManager(M,LpX,Us,Vs,Cd,Rt).

toZERO(Or,P,X,Y,M) :- correrY(Or,Y,Despues,Antes), 
		      append([],Antes,MatrizSuperior), 
		      mvX(Despues,P,X,MatrizInferior), 
		      append(MatrizSuperior,MatrizInferior,M).

%%------------------------------------------------------------------------------------

solFormat2([Name|Names],[R|Rx],[Cd|CdX],A,Sol) :- getRot(R,T), G=(Name,T,Cd), append(A,[G],Z), solFormat2(Names,Rx,CdX,Z,Sol).
solFormat2([],[],[],B,B).

isRowsNull([]).
isRowsNull([Lm|LmX]) :- Lm == o, isRowsNull(LmX).
isMatrixNull([]).
isMatrixNull([Lm|LmX]) :- isRowsNull(Lm), isMatrixNull(LmX).

circular([M|Mft],N,Z) :- length([M|Mft],U), N\=U, append(Mft,[M],Z).

%
%
%