%% el queso puede terner distintos tipos de estados, como consideraremos que el tablero va a tener quesos en todas la casillas (para facilitar la implementacion) se añadira el queso vacio. Lo definimos a continuacion:
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
% 4 = nos salimos por el oeste

rectificar((X,Y),N,M,D,0,(C,V),NewD) :- C is X, V is Y, igual(D,NewD).
rectificar((X,Y),N,M,D,1,(C,V),NewD) :- C is X, V is 0, girar(D, NewD).
rectificar((X,Y),N,M,D,2,(C,V),NewD) :- C is N, V is Y, girar(D, NewD).
rectificar((X,Y),N,M,D,3,(C,V),NewD) :- C is X, V is M, girar(D, NewD).
rectificar((X,Y),N,M,D,4,(C,V),NewD) :- C is 0, V is Y, girar(D, NewD).

igual(no,no).
igual(es,es).
igual(su,su).
igual(oe,oe).


%% cuando el raton se topa con el limite de la cudaricula, tiene que girar a la izquierda, por lo que lo modelaremos:

girar(no,oe).
girar(oe,su).
girar(su,es).
girar(es,no).

%% como el raton se va a estar moviendo, tenemos que saber que hay en cada casilla:

contiene(Tablero, (X,Y), Contenido) :- nth0(Y, Tablero, Fila), nth0(X, Fila, Contenido).

%% ya con todo esto definido, vamos a modelar las acciones del raton al recorrer el tablero:

moverRaton(Tablero, N, M, (X,Y), D, Estado, Pasos, Resultado) :- 
    verificar((X,Y), N, M,I), 
    rectificar((X,Y),N,M,D,I,(C,V),NewD), 
    contiene(Tablero, (C,V), Contenido), 
    (Estado =:= 0 -> accion(Tablero, N, M, (C,V), NewD, Estado, Contenido, Pasos, Resultado);
     accion(Tablero, N, M, (C,V), D, Estado, Contenido, Pasos, Resultado)
    ).

moverRaton(_,N,M,(X,Y),_,_,Pasos, muerto) :- verificar((X,Y),N,M,I).

moverRaton(_,N,M,(X,Y),_,_,Pasos, salio) :- verificar((X,Y),N,M,I).

accion(Tablero, N, M, (X,Y), D, Estado, vac, Pasos, Resultado) :- 
    mover((X,Y), D, (NX,NY)), random(0, 4, Rnd), 
    nth0(Rnd,[no,es,su,oe],NewD),
    mover((X,Y),NewD,(BX,BY)) ,
    (Estado =:= 0 -> moverRaton(Tablero, N, M, (NX,NY), D, Estado, [(NX,NY)|Pasos], Resultado);
     NewEstado is Estado - 1, moverRaton(Tablero, N, M, (BX,BY), NewD, NewEstado, [(BX,BY)|Pasos], Resultado)
    ).

accion(Tablero, N, M, (X,Y), D, Estado, nor, Pasos, Resultado) :- 
    mover((X,Y), D, (NX,NY)),
    (Estado =:= 0 -> moverRaton(Tablero, N, M, (NX,NY), D, Estado, [(NX,NY)|Pasos],Resultado);
     Estado >= 1, NewEstado is Estado - 1, moverRaton(Tablero,N,M,(NX,NY),D,NewEstado,[(NX,NY)|Pasos],Resultado)
    ).

accion(Tablero, N, M, (X,Y), D, Estado, ron, Pasos, Resultado) :- 
    random(0,4,Rnd), 
    nth0(Rnd,[no,su,es,oe],NewD), 
    mover((X,Y),NewD,(NX,NY)), 
    moverRaton(Tablero,N,M,(NX,NY),NewD,7,[(NX,NY)|Pasos],Resultado).

accion(Tablero, N, M, (X,Y), D, Estado, ven, Pasos, Resultado) :- 
    mover((X,Y),D,(NX,NY)),
    (Estado >= 1 -> moverRaton(Tablero,N, M, (X,Y), D, Estado, Pasos, muerto);
     moverRaton(Tablero,N,M,(NX,NY),D,Estado,[(NX,NY)|Pasos],Resultado)
    ).

accion(Tablero, N, M, (X,Y), D, Estado, fin, Pasos, Resultado) :- moverRaton(Tablero, N, M, (X,Y), D, Estado, Pasos,salio).

%% ahora juntamos todo en un solo predicado para el raton:

raton(Tablero,N,M,(X,Y),D,Pasos,Resultado) :- moverRaton(Tablero,N,M,(X,Y),D,0,[(X,Y)],Resultado),Pasos is [(X,Y)].