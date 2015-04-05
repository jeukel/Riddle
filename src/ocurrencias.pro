%-------------------------------------------------------------------------------
contarVeces(L, Res):-
    findall([X,L2], (bagof(true,member(X,L),Xs), length(Xs,L2)), Res).