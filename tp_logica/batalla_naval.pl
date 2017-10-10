%------------------Predicados predefinidos:------------------%

%fliplength(?Longitud, ?Lista)
fliplength(N, L) :- length(L, N).

%matriz(?Matriz, ?Filas, ?Columnas)
matriz(M, F, C) :- length(M, F), maplist(fliplength(C), M).

%dif1(+N1, ?N2)
dif1(N1, N2) :- N2 is N1 + 1.
dif1(N1, N2) :- N2 is N1 - 1.

%adyacente(+F1, +C1, ?F2, ?C2)
adyacente(FF, C1, FF, C2) :- dif1(C1, C2).
adyacente(F1, CC, F2, CC) :- dif1(F1, F2).
adyacente(F1, C1, F2, C2) :- dif1(C1, C2), dif1(F1, F2).

%enRango(+Matriz, +Fila, +Columna)
enRango([Fila|Filas], F, C) :-
  F > 0,
  C > 0,
  length([Fila|Filas], FMax),
    F =< FMax,
  length(Fila, CMax),
    C =< CMax.

%adyacenteEnRango(+Tablero, +F1, +C1, ?F2, ?C2)
adyacenteEnRango(T, F1, C1, F2, C2) :-
  adyacente(F1, C1, F2, C2),
  enRango(T, F2, C2).

%------------------Predicados a definir:------------------%

%contenido(+?Tablero, ?Fila, ?Columna, ?Contenido)
contenido(T, F, C, Cont) :- nth1(F, T, Fila), nth1(C, Fila, Cont).

%vacio(+?Tablero, ?Fila, ?Columna)
vacio(T, F, C) :- contenido(T, F, C, Cont), var(Cont).

%disponible(+Tablero, ?Fila, ?Columna)
disponible(T, F, C) :- vacio(T, F, C), forall(adyacenteEnRango(T, F, C, F_1, C_1), vacio(T, F_1, C_1)).

%puedoColocar(+CantPiezas, ?Direccion, +Tablero, ?Fila, ?Columna)
puedoColocar(1, _, T, F, C) :- disponible(T, F, C).
puedoColocar(N, vertical, T, F, C) :- disponible(T, F, C), N1 is N - 1, F1 is F + 1, puedoColocar(N1, vertical, T, F1, C).
puedoColocar(N, horizontal, T, F, C) :- disponible(T, F, C), N1 is N - 1, C1 is C + 1, puedoColocar(N1, horizontal, T, F, C1).

%hayBarco(+CantPiezas, ?Direccion, +Tablero, ?Fila, ?Columna)
hayBarco(1, _, T, F, C) :- contenido(T, F, C, o).
hayBarco(N, vertical, T, F, C) :- contenido(T, F, C, o), N1 is N - 1, F1 is F + 1, hayBarco(N1, vertical, T, F1, C).
hayBarco(N, horizontal, T, F, C) :- contenido(T, F, C, o), N1 is N - 1, C1 is C + 1, hayBarco(N1, horizontal, T, F, C1).

%ubicarBarcos(+Barcos, +?Tablero)
ubicarBarcos([], T).
ubicarBarcos([B|Bs], T) :- puedoColocar(B, Dir, T, F, C), hayBarco(B, Dir, T, F, C), ubicarBarcos(Bs, T).

%completarCasilleroConAgua(+Casillero, ?Resultado)
completarCasilleroConAgua(X, ~) :- \+ atom(X),!.
completarCasilleroConAgua(o, o).

%completarConAgua(+Original, ?Completo)
completarFilaConAgua(X, Y) :- maplist(completarCasilleroConAgua, X, Y).

%completarConAgua(+?Tablero)
completarConAgua(X, Y) :- maplist(completarFilaConAgua, X, Y).

%reemplazar(+Lista, +Indice, +Elemento, -Resultado)
reemplazar([], _, _, []).
reemplazar([X|Xs], 1, Z, [Z|Xs]).
reemplazar([X|Xs], N, Z, [X|Ys]) :- N > 1, N1 is N - 1, reemplazar(Xs, N1, Z, Ys).

%golpear(+Tablero, +NumFila, +NumColumna, -NuevoTab)
golpear(T, F, C, T) :- contenido(T, F, C, ~).
golpear(T, F, C, NuevoT) :- contenido(T, F, C, o), nth1(F, T, Fila),
                            reemplazar(Fila, C, ~, NuevaFila),
                            reemplazar(T, F, NuevaFila, NuevoT).

% Completar instanciación soportada y justificar.
%atacar(Tablero, Fila, Columna, Resultado, NuevoTab)

%------------------Tests:------------------%

test(1) :-
  matriz(M, 2, 3),
  adyacenteEnRango(M, 2, 2, 2, 3).
test(2) :-
  matriz(M, 2, 3),
  setof(
    (F, C),
    adyacenteEnRango(M, 1, 1, F, C),
    [(1, 2), (2, 1), (2, 2)]
  ).
tests :- forall(between(1, 2, N), test(N)). % Cambiar el 2 por la cantidad de tests que tengan.
