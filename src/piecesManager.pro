%Test:compPiece([[x,x,x,x],[x,x,x,x],[x,x,o,x]],[[x,x,x],[x,o,x]],1,1,X,Y),correrY([[x,x,x,x],[x,x,x,x],[x,x,o,x]],Y,Despues,Antes),append([],Antes,MatrizSuperior),mvX(Despues,[[x,x,x],[x,o,x]],X,MatrizInferior),append(MatrizSuperior,MatrizInferior,MatrizCompleta),NL=[X,Y],append([],NL,W).
%Recursive function for comparing pieces
piecesManager(_,L,Y,X,X,Y):-length(L,1).
piecesManager(_, [ _ | [[Lp|LpX]] ],T2,T,T,T2):-Lp==[],LpX==[].
piecesManager(Or, [Lp|LpX],T2,T,Z,ListaDeRotaciones) :- 
	print(Lp),print(LpX),print(T2),print(T),print(Or),nl,
	compPiece(Or,Lp,1,1,X,Y),
	correrY(Or,Y,Despues,Antes),
	append(T2,Antes,MatrizSuperior),
	mvX(Despues,Lp,X,MatrizInferior),
	append(MatrizSuperior,MatrizInferior,MatrizCompleta),
	NL= [X,Y] ,
	append(T,[NL],W),
	print(Lp),print(LpX),print(T2),print(T),print(Or),nl,
	piecesManager(MatrizCompleta,[LpX],T2,W,Z,ListaDeRotaciones);
	print(Lp),print(LpX),print(T2),print(T),print(Or),nl,
	rotarRandom(LpX,Num,PiezasRotadas),
	append(T2,[Num],ListaDeRot),
	piecesManager(Or,PiezasRotadas,ListaDeRot,T2,Z,ListaDeRotaciones).
	%piecesManager([[x,x,x,x],[x,x,x,x],[x,x,o,x]],[[a],[[[x,x,x],[x,o,x]],[x,x,x]]],[],[],A,B).
%piecesManager(Or,[LnP|[Lp|LpX]],T,RotLT,RotList,Z):-(norepeat([Lp|LpX]),compPiece(Or,Lp,[],[],X,Y),NL= [X,Y],append(T,NL,W),piecesManager(Or, [LnP | LpX],W,Z)); rotarRandom([Lp|LpX],[],Num,Res),append(RotLT,Num,NRTL),piecesManager(Or, Res,T,NRTL,RotList,Z).
%compPiece(Or,Lp,[],[],X,Y),NL= [X,Y],append(T,NL,W),piecesManager(Or, [LnP | LpX],W,Z)); rotarRandom([Lp|LpX],[],Num,Res),append(RotLT,Num,NRTL),piecesManager(Or, Res,T,NRTL,RotList,Z).