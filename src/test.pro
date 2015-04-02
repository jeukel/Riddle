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

invertir([],[],[]).
invertir([],L1,L1).	
invertir([L|C],Temp,Res):- invertir(C , [L|Temp], Res).



/*resolver(x):-generar(x),probar(x).*/

resolver([] , _ , _ ):- fail. %Si le pasa una figura vacía,falla.
resolver(M1, [], Sol):- M1 == Sol.%Si la lista de piezas está vacía, falla.
resolver(M1 , LMs , Sol):- generar(LMs,Sol), resolver(M1 ,LMs,Sol).

/* Hay que ir armando una lista de piezas*/
/* El metodo principal tiene que recibir una  matriz y una lista de matrices*/

notImplies(X1,X2,R):- (X1==x,X2==x,R = 0);(X1==0,X2==0,R = 0);
	 (X1==x,X2==0,R = x); (X1==0,X2==x,R = 0).
	 

elimColumn(M1,Mres):- rait(M1,[],Res), invertir(Res,[],Mres).
rait([],M,M).
rait([H|C],T,Res):-quitarCabeza(H,HRes),rait(C,[HRes|T],Res). 

quitarCabeza([_|C],C).