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

%-------------------------------------------------------------------------------
%TABLA DE VERDAD
%-------------
%| P | Q | R |
%| x | x | o |
%| o | o | o |
%| x | o | x |
notImplies(X1,X2,R):- (X1==x,X2==x,R = o);(X1==o,X2==o,R = o);
	 (X1==x,X2==o,R = x).
%-------------------------------------------------------------------------------

elimColumn(M1,Mres):- rait(M1,[],Res), reverse(Res,Mres2),Mres = Mres2.

rait([],M,M).
rait([H|C],T,Res):-quitarCabeza(H,HRes),rait(C,[HRes|T],Res). 

quitarCabeza([_|C],C).
tomarCabeza([X|_],X).

%-------------------------------------------------------------------------------
%Parte una lista en el indice dado.
correrY([],Y,Y2,X,Y2):- Y\=[],Y2\=[],reverse(Y,Z),X=Z.
correrY([M|Mc],1,Mres2,Mres):- Mres=[],Mres2=[M|Mc].
correrY([M|Mc],Y,Mres2,Mres):-correrY([M|Mc],Y,1,[],[],Mres2,Mres).

correrY([M|Mc],Y,YT,T,T2,Mres2,Mres):-
	(YT<Y,
	Yt is YT +1,
	correrY(Mc,Y,Yt,[M|T],Mc,Mres2,Mres)
	)
	;correrY([],T,T2,Mres,Mres2).

%-------------------------------------------------------------------------------
zeroFilln([M|Mc],[Mp|MpC],T,Res):-
	compPiece([M|Mc],[Mp|MpC],1,1,X,Y),
	correrY([M|Mc],Y,Res1,Res2),
	append(T,Res1,T2) ,
	mvX(Res2,[Mp|MpC],X,Res3),
	append(T2,Res3,Res4),
	Res=Res4. 
	
%----------------------------------------------------------------
%Testing purposes : rotarRandom([[a,b,c],[c,d,e],[e,f,g]],[],Num,Res).
rotarRandom(ListaPcs,Num,Res):-rotarRandom(ListaPcs,[],Num,Res).
rotarRandom([LPcs|LpcsC],T,Num,Res):-
	length([LPcs|LpcsC],X),
	random_between(1,X,Num),
	correrY([LPcs|LpcsC],Num,Res2,Res3),
	append(T,Res3,Res4),
	quitarCabeza(Res2,ColaRes2),
	tomarCabeza(Res2,CabezaRes2),
	rotarMatriz(CabezaRes2,[],PiezaRotada),
	append(Res4,[PiezaRotada],Res5),
	append(Res5,ColaRes2,Res).

%--------------------------------------------------------------------------------

%Remplaza elimina pieza de figura original (listas)
repElem(A,[],U,Z) :- append(U,A,Z).
repElem([Lm|LmX],[Lp|LpX], U, Z) :- notImplies(Lm,Lp,D), append(U,[D],T),repElem(LmX,LpX,T,Z).
	 
%--------------------------------------------------------------------------------
matrixByRow(_,[],Z,Z).
matrixByRow([Lm|LmX],[Lp|LpX],U, Z) :- repElem(Lm,Lp,[],T), append(U,[T],W), matrixByRow(LmX,LpX,W,Z).

%Matriz, Pieza, ContadorEnX, Cabeza, Resto.
mvX(M,P,Nx,Z) :- mvColumn(M,Nx,[],[],H,R), matrixByRow(R,P,[],U), unify(H,U,[],Z).

unify([],[],Z,Z).
unify([M1|Mx],[M2|Ms],N,Z) :- append(M1,M2,N1), append(N,[N1],T), unify(Mx,Ms,T,Z).

mvColumn([],_,T1,T2,R,H) :- reverse(T1,H), reverse(T2,R).
mvColumn([Ml|Mlx],Nx,T1,T2,H,R) :- correrY(Ml,Nx,W1,W2), append([W1],T1,Z1), append([W2],T2,Z2), mvColumn(Mlx,Nx,Z1,Z2,H,R).

%------------------------------------------------------------------------------------

norepeat([]).
norepeat([Lp|LpX]) :- norepeat_aux(Lp,LpX),repeatWrot(Lp,LpX,1), norepeat(LpX).
repeatWrot(_,_,N) :- N == 4.
repeatWrot(H,LpX,N) :- rotarMatriz(H,[],Z), print(Z), T is N+1, norepeat_aux(Z,LpX), repeatWrot(Z,LpX,T).

norepeat_aux(_,[]).
norepeat_aux(H,[R|Rx]) :- H\=R, norepeat_aux(H,Rx).

%-------------------------------------------------------------------------------

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
compMatrix([Lm|LmX],[Lp|LpX],T, Y) :-
	(compRows([Lm|LmX],[Lp|LpX]),
	compMatrix([],[],T,Y));
	R is T+1,
	compMatrix(LmX,[Lp|LpX],R,Y).

compMatrix([],X,X).
compMatrix(Lm,Lp, Y):- compMatrix(Lm,Lp,1,X),compMatrix([],X,Y).

compPiece([],_,X,Y,X,Y).
compPiece(_,[],X,Y,X,Y).
compPiece([],[],X,Y,X,Y).
compPiece([Lm|LmX],[Lp|LpX],T1,T2,X,Y) :-
	(compMatrix([Lm|LmX],[Lp|LpX],T3),compPiece([],[Lp|LpX],T1,T3,X,Y));
	(elimColumn([Lm|LmX],Mres),Xt is T1 + 1,compPiece(Mres,[Lp|LpX],Xt,T2,X,Y)).
	
compPiece(Matriz,Pieza,Xt,Yt,Rt,Rot,X,Y):-(compPiece(Matriz,Pieza,Xt,Yt,X,Y)),Rot=Rt;
	 (Rt\=4,rotarMatriz(Pieza,[],PiezaRotada),Rts is Rt +1,compPiece(Matriz,PiezaRotada,Xt,Yt,Rts,Rot,X,Y)).

compPiece(Matriz,Pieza,Rot,X,Y):-compPiece(Matriz,Pieza,1,1,0,Rot,X,Y).

%%------------------------------------------------------------------------------------
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

getRot(A,B) :- A>=4 , T is A-4, getRot(T,B).
getRot(0,0).
getRot(1,90).
getRot(2,180).
getRot(3,270).

%------------------------------------------------------------------------------------

solFormat([Name|Names],Rots,A,B) :- getQuant([Name|Names],Rots,[],W), W=[U|Us], getRot(U,T), G=(Name,T), append(A,[G],Z), solFormat_aux(Names,Us,Z,B).
solFormat_aux([Name|Names],[R|Rl],A,B) :- getRot(R,T), G=(Name,T), append(A,[G],Z), solFormat_aux(Names,Rl,Z,B).
solFormat_aux([],[],B,B).
	
%-------------------------------------------------------------------------------
%Funcion que devuelva una lista de rotaciones
%-------------------------------------------------------------------------------

%Function for getting props of pieces
getProps([Name|List],Z) :- append([],[Name],A), append([],List,B), [A|[B]] = Z.
getPropsRec([[Name|List]|_],[Z|[Zx|_]],Lf) :- append(Z,[Name],A), append(Zx,List,B), Lf = [A|[B]].

%Recursive function for getting props of pieces
pieceS([],X,X).
pieceS([Lp|LpX],U,Z) :- getPropsRec([Lp],U,Lf), pieceS(LpX,Lf,Z).

%------------------------------------------------------------------------------------

%Do
%figures([Lp|LpX], Z) :- getProps(Lp,U), pieceS(LpX,U,Z).
figures(Or, [Lp|LpX], Z) :- getProps(Lp,U), pieceS(LpX,U,Z), piecesManager(Or,Z,[],K).

