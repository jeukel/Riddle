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
compMatrix(Lm,Lp, Y):- compMatrix(Lm,Lp,0,X),compMatrix([],X,Y).
