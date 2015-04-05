modificarListaRot(LPcs,Pieza,LRots,Rot,LRN):-nth1(Index,LPcs,Pieza),
	correrY(LRots,Index,Despues,Antes),append([],Antes,NL),quitarCabeza(Despues,Dcola),
	append(NL,[Rot],NL2),append(NL2,Dcola,LRNs),LRN=LRNs.
	
	
correrY([],Y,Y2,X,Y2):- Y\=[],Y2\=[],reverse(Y,Z),X=Z.
correrY([M|Mc],1,Mres2,Mres):- Mres=[],Mres2=[M|Mc].
correrY([M|Mc],Y,Mres2,Mres):-correrY([M|Mc],Y,1,[],[],Mres2,Mres).

correrY([M|Mc],Y,YT,T,T2,Mres2,Mres):-
	(YT<Y,
	Yt is YT +1,
	correrY(Mc,Y,Yt,[M|T],Mc,Mres2,Mres)
	)
	;correrY([],T,T2,Mres,Mres2).
	
quitarCabeza([_|C],C).