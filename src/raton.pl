%% el queso puede terner distintos tipos de estados, 
%% como consideraremos que el tablero va a tener quesos en todas la casillas (para facilitar la implementacion) se añadira el queso vacio. 
%% Lo definimos a continuacion:
% - nor = Normal
% - ron = Ron
% - ven = Veneno
% - vac = Vacio
% - fin = salida

queso(nor).
queso(ron).
queso(ven).
queso(vac).
queso(fin).

%% el raton puede estar viendo hacia distintos lados, que son:
% - no = Norte
% - su = Sur
% - es = Este
% - oe = Oeste

pos(no).
pos(su).
pos(es).
pos(oe).

%% tenemos que el raton se puede mover en linea recta adentro de la cuadricula, lo que se modela de la siguiente forma:

mover((X,Y), no, (X,Y1)) :- Y1 is Y - 1.
mover((X,Y), su, (X,Y1)) :- Y1 is Y + 1.
mover((X,Y), es, (X1,Y)) :- X1 is X + 1.
mover((X,Y), oe, (X1,Y)) :- X1 is X - 1.

%% como nos podemos salir de la cuadricula si no tenemos quidado, verificaremos que siempre estemos en la cuadricula:

verificar((X,Y),N,M,Z) :- (N<X -> Z is 2;(X<0 -> Z is 4 ; (M < Y -> Z is 3;(Y < 0 -> Z is 1; Z is 0)))).

%% calculamos el index, el cual nos va a decir si nos hemos salido y en que direccion fue
% 0 - seguimos adentro de la cuadricula
% 1 - nos salimos por el norte
% 2 - nos salimos por el este
% 3 - nos salimos por el sur
% 4 - nos salimos por el oeste

rectificar((X,Y), _, _, D, 0, (C,V), NewD) :- C is X, V is Y, igual(D, NewD).
rectificar((X,_), _, _, D, 1, (C,V), NewD) :- C is X, V is 0, girar(D, NewD).
rectificar((_,Y), N, _, D, 2, (C,V), NewD) :- C is N, V is Y, girar(D, NewD).
rectificar((X,_), _, M, D, 3, (C,V), NewD) :- C is X, V is M, girar(D, NewD).
rectificar((_,Y), _, _, D, 4, (C,V), NewD) :- C is 0, V is Y, girar(D, NewD).

igual(no,no).
igual(es,es).
igual(su,su).
igual(oe,oe).


%% cuando el raton se topa con el limite de la cudaricula, tiene que girar a la izquierda, por lo que lo modelaremos:

girar(no,oe).
girar(oe,su).
girar(su,es).
girar(es,no).

%% Creamos un predicado que nos permita movernos de forma segura sin salirnos del tablero:
% - D = Direccion a la que mira el raton
% - N = tamaño maximo del tablero en el eje X
% - M = tamaño maximo del tablero en el eje Y
% - Estado = nos dice si el raton esta borracho o no, 0 es sobrio y cualquier n > 0 es ebrio (max. 7)


movSec((X,Y), D, N, M, Estado, (NX,NY),ND,R) :- 
    mover((X,Y), D, (C,V)),
    verificar((C,V), N, M, Z),
    rectificar((C,V), N, M, D, Z, (NC,NV), NewD),
    (Estado < 1 -> NX is NC, NY is NV, igual(NewD,ND);
        NX is NC, NY is NV, igual(D,ND) 
    ),
    R is Z.

%% como el raton se va a estar moviendo, tenemos que saber que hay en cada casilla:

contiene(Tablero, (X,Y), Contenido) :- nth0(Y, Tablero, Fila), nth0(X, Fila, Contenido).

%% ya con todo esto definido, vamos a modelar las acciones del raton al recorrer el tablero,
% lo que hacemos es revisar que contiene la casilla en la que esta el raton y evaluar dependiendo de lo que haya y de si esta borracho o no

moverRaton(Tablero, (X,Y), N, M, D, Estado, Pasos, Resultado) :- 
    contiene(Tablero, (X,Y), Contenido),
    accion(Tablero, (X,Y), N, M, D, Estado, Contenido, Pasos, Resultado).


%% se modelan las acciones que puede realizar el raton dependiendo del tipo de queso y su estado

% acciones posibles si la casilla esta vacia
accion(Tablero, (X,Y), N, M, D, Estado, vac, Pasos, Resultado):-
    random(0, 4, Rnd),
    nth0(Rnd, [no,su,es,oe], RD), 
    movSec((X,Y), D, N, M, Estado, (NX,NY), ND, R),
    (Estado < 1 -> moverRaton(Tablero, (NX,NY), N, M, ND, Estado, [(X,Y)|Pasos], Resultado);
    (R > 0 -> NewEstado is Estado - 1, moverRaton(Tablero, (NX,NY), N, M, ND, NewEstado, [(X,Y)|Pasos], Resultado);
        NewEstado is Estado - 1, moverRaton(Tablero, (NX,NY), N, M, RD, NewEstado, [(X,Y)|Pasos], Resultado) 
     ) 
    ).
% acciones posibles si la casilla tiene un queso normal    
accion(Tablero, (X,Y), N, M, D, Estado, nor, Pasos, Resultado):-
    comerQueso(Tablero, Y, X, vac, NewTablero), 
    movSec((X,Y), D, N, M, Estado, (NX,NY),ND,_),
    (Estado < 1 -> moverRaton(NewTablero, (NX,NY), N, M, ND, Estado, [(X,Y)|Pasos],Resultado);
    NewEstado is Estado - 1, moverRaton(NewTablero, (NX,NY), N, M, ND, NewEstado, [(X,Y)|Pasos],Resultado) 
    ).
% si el queso tiene ron entonces se enborracha
accion(Tablero, (X,Y), N, M, _, Estado, ron, Pasos, Resultado):-
    random(0, 4, Rnd),
    nth0(Rnd, [no,su,es,oe], RD),  
    movSec((X,Y), RD, N, M, Estado, (NX,NY),ND,_),
    comerQueso(Tablero, Y, X, vac, NewTablero),
    moverRaton(NewTablero, (NX,NY), N, M, ND, 7, [(X,Y)|Pasos], Resultado).

% acciones posbles si el raton llega a un queso envenenado
% si el raton muere, entonces se llama a la accion de terminar el recorrido

accion(Tablero, (X,Y), N, M, D, Estado, ven, Pasos, Resultado):-
    movSec((X,Y), D, N, M, Estado, (NX,NY), ND,_),
    comerQueso(Tablero, Y, X, vac, NewTablero),
    (Estado < 1 -> moverRaton( Tablero, (NX,NY), N, M, ND, Estado, [(X,Y)|Pasos], Resultado);
        accion(NewTablero,(X,Y),N,M,D,Estado,aux2,[(X,Y)|Pasos],Resultado)
    ).
% si el raton llega a la casilla de salida, entonces se llama a la accion de terminar el recorrido
accion(Tablero, (X,Y), N, M, D, Estado, fin, Pasos , Resultado) :- 
    accion( Tablero, (X,Y), N, M, D, Estado, aux, [(X,Y)|Pasos], Resultado).

%% una vez que el raton ha termiando su recorrido, el programa se terminara imprimendo la lista con los pasos que dio
% y asignara el resultado como 'salio' en caso de que haya llegado a la salida, o 'muerto' en caso de que se comiera el queso envenenado

accion(_, _, _, _, _, _, aux, Pasos, Resultado):- 
    set(2, Resultado),writeln(Pasos).

accion(_, _, _, _, _, _, aux2, Pasos, Resultado):- 
    set(1, Resultado),writeln(Pasos).

set(1, muerto).
set(2, salio).


% Reemplazar un elemento en una lista
reemplazar([_|T], 0, NewElem, [NewElem|T]).
reemplazar([H|T], Pos, NewElem, [H|R]) :-
    Pos > 0,
    Pos1 is Pos - 1,
    reemplazar(T, Pos1, NewElem, R).

% Reemplazar un elemento en una matriz, en este caso simula que el raton se comio el queso en la posicion (X,Y)
comerQueso([Row|RestRows], 0, Col, NewElem, [NewRow|RestRows]) :-
    reemplazar(Row, Col, NewElem, NewRow).
comerQueso([Row|RestRows], RowNum, Col, NewElem, [Row|NewRestRows]) :-
    RowNum > 0,
    RowNum1 is RowNum - 1,
    comerQueso(RestRows, RowNum1, Col, NewElem, NewRestRows).

%% predicado principal, es que que va a recibir el tablero, la posicion del raton, la direccion a la que ve y las dimenciones de la matriz.
raton(Tablero, (X,Y), N, M, D, Resultado) :- moverRaton(Tablero, (X,Y), N-1, M-1, D, 0, [], Resultado).
