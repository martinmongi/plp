% para recargar el fuente con "r." en vez de "[batalla_naval]."
r :- [batalla_naval].

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
disponible(T, F, C) :- vacio(T, F, C),
                       forall(adyacenteEnRango(T, F, C, F_1, C_1),
                       vacio(T, F_1, C_1)).

direccion(vertical).
direccion(horizontal).

%puedoColocar(+CantPiezas, ?Direccion, +Tablero, ?Fila, ?Columna)
puedoColocar(1, D, T, F, C) :- nonvar(D), direccion(D), disponible(T, F, C).
puedoColocar(1, D, T, F, C) :- var(D), disponible(T, F, C).

puedoColocar(N, vertical, T, F, C)   :- N > 1, puedoColocarAux(N, T, F, C, 1, 0).
puedoColocar(N, horizontal, T, F, C) :- N > 1, puedoColocarAux(N, T, F, C, 0, 1).

puedoColocarAux(0, _, _, _, _, _).
puedoColocarAux(N, T, F, C, Df, Dc) :-  disponible(T, F, C),
                                        N1 is N - 1,
                                        F1 is F + Df, C1 is C + Dc,
                                        puedoColocarAux(N1, T, F1, C1, Df, Dc).

%hayBarco(+CantPiezas, ?Direccion, +Tablero, ?Fila, ?Columna)
hayBarco(1, _, T, F, C)          :- contenido(T, F, C, o).
hayBarco(N, vertical, T, F, C)   :- contenido(T, F, C, o),
                                    N1 is N - 1, F1 is F + 1,
                                    hayBarco(N1, vertical, T, F1, C).
hayBarco(N, horizontal, T, F, C) :- contenido(T, F, C, o),
                                    N1 is N - 1, C1 is C + 1,
                                    hayBarco(N1, horizontal, T, F, C1).

%ubicarBarcos(+Barcos, +?Tablero)
ubicarBarcos([], _).
ubicarBarcos([B|Bs], T) :- puedoColocar(B, Dir, T, F, C),
                           hayBarco(B, Dir, T, F, C),
                           ubicarBarcos(Bs, T).

%completarConAgua(+?Tablero)
completarConAgua(T) :- maplist(completarFilaConAgua, T).

%completarFilaConAgua(+?Fila)
completarFilaConAgua(F) :- maplist(completarCasilleroConAgua, F).

%completarCasilleroConAgua(+?Casillero)
completarCasilleroConAgua(~) :- !.
completarCasilleroConAgua(o).

%reemplazar(+Lista, +Indice, +Elemento, -Resultado)
reemplazar([],     _, _, []).
reemplazar([_|Xs], 1, Z, [Z|Xs]).
reemplazar([X|Xs], N, Z, [X|Ys]) :- N > 1, N1 is N - 1,
                                    reemplazar(Xs, N1, Z, Ys).

%golpear(+Tablero, +NumFila, +NumColumna, -NuevoTab)
golpear(T, F, C, T)      :- contenido(T, F, C, ~).
golpear(T, F, C, NuevoT) :- contenido(T, F, C, o), nth1(F, T, Fila),
                            reemplazar(Fila, C, ~, NuevaFila),
                            reemplazar(T, F, NuevaFila, NuevoT).

golpeaUnBarco(T, F, C, NuevoT) :- contenido(T, F, C, o),
                                  golpear(T, F, C, NuevoT).

%atacar(+Tablero, +Fila, +Columna, -Resultado, -NuevoTab)
atacar(T, F, C, agua,    T)      :- golpear(T, F, C, T).
atacar(T, F, C, hundido, NuevoT) :- golpeaUnBarco(T, F, C, NuevoT),
                                    forall(adyacenteEnRango(T, F, C, F_1, C_1),
                                           contenido(T, F_1, C_1, ~)).
atacar(T, F, C, tocado,  NuevoT) :- golpeaUnBarco(T, F, C, NuevoT),
                                    adyacenteEnRango(T, F, C, F_1, C_1),
                                    contenido(T, F_1, C_1, o).

% Ejercicio 8: es reversible el predicado atacar en alguno de sus parámetros?

/**

# Caso T instanciado

  Es reversible!

  Mientras el tablero original (T) esté instanciado, podemos dejar libres el
  resto de las variables (Fila, Columna, Resultado y NuevoTab) y obtendremos
  todos los ataques posibles a ese tablero.

  Ésto es porque atacar usa golpear, que usa contenido, que usa nth1, y nth1
  puede recorrer los elementos de una lista -- además es reversible en todos sus
  argumentos: nth1(?Index, ?List, ?Elem) -- entonces las coordenadas se van a ir
  instanciando en los lugares posibles y veremos todos los casos.

  También podemos agregar (una de/varias de/todas) las otras variables (Fila,
  Columna, Resultado, NuevoTab) y el atacar será verdadero en los casos
  correctos.

  Podríamos mejorar el hint de instanciación a:

  %atacar(+Tablero, ?Fila, ?Columna, ?Resultado, ?NuevoTab)

  que es más general que el original:

  %atacar(+Tablero, +Fila, +Columna, -Resultado, -NuevoTab)

# Caso T sin instanciar:

 ## NuevoTab sin instanciar:

  ## Instanciando sólo coordenadas y Res

    Salvo cuando Res = agua, no es reversible (se cuelga).

    ### "agua"

      Si además de las coordenadas pedimos "agua" funciona OK; vemos el caso
      donde el tablero tiene agua en esa posicion (y el resto de las celdas sin
      instanciar), y el nuevo tablero es el mismo que el original. Si pedimos
      mas resultados, da false.

        ?- atacar(T, 1, 2, agua, NuevoT).
        T = NuevoT, NuevoT = [[_G3028, ~|_G3032]|_G3026] ;
        false.

    ### "hundido"

      Si pedimos "hundido", se cuelga sin dar resultados:

        ?- atacar(T, 1, 2, hundido, NuevoT).
        (... pasa un rato...)
        ERROR: Out of global stack

        TODO: por que?

    ### "tocado"

      Con un ejemplo trivial (claramente falso) se cuelga:

        ?- atacar(T, F, C, tocado, [[o]]).
        (... pasa muuuuucho tiempo sin output y me aburro de esperar)

        TODO: por que?

  ## Instanciando solo coordenadas

    Prolog prueba instanciar Res, en el orden en que los definimos. Entra a
    "agua", da el resultado trivial, y después se cuelga en "hundido".

  ## Instanciando solo Res

    TODO: ?

  ## Nada instanciado?

    No es reversible. Primero instancia Res a "agua" y luego usa nth1 (via
    golpear->contenido->nth1) con el tablero sin instanciar, entonces nunca se
    le acaban las columnas:

      ?- atacar(T, F, C, R, NT).
      T = NT, NT = [[~|_G8737]|_G8734],
      F = C, C = 1,
      R = agua ;
      T = NT, NT = [[_G8736, ~|_G8740]|_G8734],
      F = 1,
      C = 2,
      R = agua ;
      T = NT, NT = [[_G8736, _G8739, ~|_G8743]|_G8734],
      F = 1,
      C = 3,
      R = agua ;

      (...sigue dando casos similares donde C y los tableros crecen)

    ...TODO...

 ## Tablero sin instanciar, NuevoTab instanciado:
   ...TODO...

  ### Mirando una coordenada particular de un NuevoTab
    ...TODO...

  ### Mirando un Resultado particular de un NuevoTab
    ...TODO...
*/

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
test(3) :-
  contenido([[o,~]], 1, 1, o),
  contenido([[o,~]], 1, 2, ~).
test(4) :-
  matriz(M, 1, 1),
  vacio(M, 1, 1).
test(5) :-
  matriz(M, 3, 3),
  contenido(M, 1, 2, o),
  setof(
    (F, C),
    disponible(M, F, C),
    [(3, 1), (3, 2), (3, 3)]
  ).
test(6) :-
  matriz(M, 2, 4),
  setof(
    (F, C, Dir),
    puedoColocar(3, Dir, M, F, C),
    [(1, 1, horizontal), (1, 2, horizontal), (2, 1, horizontal), (2, 2, horizontal)]
  ).
test(7) :-
  matriz(M,2,3),
  contenido(M,2,1,o),
  setof(
    (F, C, Dir),
    puedoColocar(2,Dir,M,F,C),
    [(1, 3, vertical)]
  ).
test(8) :-
  matriz(M,3,2),
  setof(
    M,
    ubicarBarcos([2,1],M),
    [
      [[o, o], [_, _], [o, _]],
      [[o, o], [_, _], [_, o]],
      [[o, _], [_, _], [o, o]],
      [[_, o], [_, _], [o, o]]
    ]
  ).
test(9) :-
  matriz(M,1,2),
  contenido(M,1,1,~),
  contenido(M,1,2,o),
  golpear(M,1,1,IntM),
  golpear(IntM,1,2,NuevaM),
  contenido(NuevaM,1,1,~),
  contenido(NuevaM,1,2,~).

test(10) :-
  Tablero = [[o, o], [_, _], [_, o]],
  Tablero \== [[o, o], [~, ~], [~, o]],
  completarConAgua(Tablero),
  Tablero == [[o, o], [~, ~], [~, o]].

% Cambiar el 2 por la cantidad de tests que tengan.
tests :- forall(between(1, 10,  N), test(N)).
