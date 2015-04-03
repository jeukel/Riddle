
correrY([],Y,Y2,X,Y2):- Y\=[],Y2\=[],reverse(Y,Z),X=Z.

correrY([M|Mc],Y,Mres2,Mres):-correrY([M|Mc],Y,1,[],[],Mres2,Mres).

correrY([M|Mc],Y,YT,T,T2,Mres2,Mres):-YT<Y,Yt is YT +1, correrY(Mc,Y,Yt,[M|T],Mc,Mres2,Mres); correrY([],T,T2,Mres,Mres2).

