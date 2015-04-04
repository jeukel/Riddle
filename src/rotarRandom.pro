rotarRandom([LPcs|LpcsC],T,Res):-length([LPcs|LpcsC],X),random_between(0,X,Y),correrY([LPcs|LpcsC],Y,Res2,Res3),
	append(T,Res3,Res4),quitarCabeza(Res2,ColaRes2),append(Res4,ColaRes2,Res5),tomarCabeza(Res2,CabezaRes2),
	rotarMatriz(CabezaRes2,[],PiezaRotada),append(Res5,[PiezaRotada],Res).

correrY([],Y,Y2,X,Y2):- Y\=[],Y2\=[],reverse(Y,Z),X=Z.

correrY([M|Mc],Y,Mres2,Mres):-correrY([M|Mc],Y,1,[],[],Mres2,Mres).

correrY([M|Mc],Y,YT,T,T2,Mres2,Mres):-YT<Y,Yt is YT +1, correrY(Mc,Y,Yt,[M|T],Mc,Mres2,Mres); correrY([],T,T2,Mres,Mres2).

tomarCabeza([X|_],X).
quitarCabeza([_|C],C).

rotarMatriz([],L1,L1).
rotarMatriz([M|C], Temp , Res):- not( is_list(M)),tomarLista([M|C],Temp,[],A),invertir(A,[],B),rotarMatriz([],B,Res).
rotarMatriz([M|C], Temp , Res):- is_list(M),tomarLista(M , Temp , [], A),invertir(A , [], B),
	rotarMatriz(C,B,Res).

tomarLista([],[],L1,L1).
tomarLista([MH|MC], [], Lista, Res):-tomarLista(MC,[], [[MH]|Lista],Res).
tomarLista([MH|MC],[Lista2|L2C],Lista, Res):- tomarLista(MC,L2C, [[MH|Lista2]|Lista],Res).

invertir([],[],[]).
invertir([],L1,L1).	
invertir([L|C],Temp,Res):- invertir(C , [L|Temp], Res).

