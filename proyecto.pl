:- op(300,fx,~). % negacion, prefija, no asociativa.
:- op(400,yfx,(/\)). % conjuncion, infija, asociativa a izquierda.
:- op(500,yfx,(\/)). % disyuncion, infija, asociativa a izquierda.
:- op(600,xfx,=>). % implicacion, infija, no asociativa.
:- op(650,xfx,<=>). % equivalencia, infija, no asociativa.

/* ELIMINO TODAS LAS IMPLICACIONES DE MI FBF */
transformarImplicaciones(P,P):- atomic(P).
transformarImplicaciones(P,(RTA)):-transformarImplicaciones(P,RTA).
transformarImplicaciones(P \/ Q, (P1) \/ (Q1) ):-
    transformarImplicaciones(P,P1),
    transformarImplicaciones(Q,Q1).
transformarImplicaciones( P => Q, P1 \/ Q1 ):- 
    transformarImplicaciones((~P),P1),
    transformarImplicaciones(Q,Q1).
transformarImplicaciones( P /\ Q, (P1) /\ (Q1) ):-
    transformarImplicaciones(P,P1),
    transformarImplicaciones(Q,Q1).
transformarImplicaciones(P <=> Q, (P1) <=> (Q1) ):-
    transformarImplicaciones(P,P1),
    transformarImplicaciones(Q,Q1).

/* ELIMINO TODAS LAS EQUIVALENCIAS DE MI FBF */
transformarEquivalencias(P,P):- atomic(P).
transformarEquivalencias(P,(RTA)):- transformarEquivalencias(P,RTA). 
transformarEquivalencias( P <=> Q, ((P1)\/(Q1)) /\ ((Q1) \/ (P1)) ):-
    transformarEquivalencias(P,P1),
    transformarEquivalencias(Q,Q1).
transformarEquivalencias(P \/ Q, (P1) \/ (Q1) ):-
    transformarEquivalencias(P,P1),
    transformarEquivalencias(Q,Q1).
transformarEquivalencias( P /\ Q, (P1) /\ (Q1) ):-
    transformarEquivalencias(P,P1),
    transformarEquivalencias(Q,Q1).
transformarEquivalencias(P => Q, (P1) => (Q1)):-
    transformarEquivalencias(P,P1),
    transformarEquivalencias(Q,Q1).

/* ELIMINO LAS IMPLICACIONES Y EQUIVALENCIAS DE MI FBF */
fBienEscrita(F,RTA):- 
    transformarImplicaciones(F,Rta1),
    transformarEquivalencias(Rta1,RTA).

/* PRIMER PASO PARA TRANSFORMAR A FNCR */
fPaso1(~top, bottom).
fPaso1(~bottom, top).
fPaso1(P,P):- atomic(P).
fPaso1( ~P, ~P):- atomic(P).
fPaso1( ~(~P) , P1 ):- fPaso1(P,P1).
fPaso1( ~(~P) , P ):- atomic(P).
fPaso1(~(P \/ Q), (P1) /\ (Q1) ):-
    fPaso1(~P,P1),
    fPaso1(~Q,Q1).
fPaso1(~(P /\ Q), (P1) \/ (Q1) ):-
    fPaso1(~P,P1),
    fPaso1(~Q,Q1).
fPaso1(P \/ Q, (P1) \/ (Q1) ):-
    fPaso1(P, P1),
    fPaso1(Q,Q1).
fPaso1(P /\ Q, (P1) /\ (Q1) ):-
    fPaso1(P,P1),
    fPaso1(Q,Q1).

/* SEGUNDO PASO PARA TRANSFORMAR A FNCR */
fPaso2(P,P):- atomic(P).
fPaso2(~P,~P):- atomic(P).
fPaso2( P1 \/ (P2 /\ P3), (P01) /\ (P02) ):-
    fPaso2(P1 \/ P2,P01),
    fPaso2(P1 \/ P3,P02).
fPaso2( (P1 /\ P2) \/ P3, (P01) /\ (P02) ):-
    fPaso2(P1 \/ P3, P01),
    fPaso2(P2 \/ P3, P02).
fPaso2( P1 /\ P2, (P01) /\ (P02)):-
    fPaso2(P1,P01),
    fPaso2(P2,P02).
fPaso2( P1 \/ P2, (P01) \/ (P02)):-
    fPaso2(P1,P01),
    fPaso2(P2,P02).
fPaso2(_P \/ top, top ).
fPaso2(top \/ _P, top ).
fPaso2(P \/ bottom, P).
fPaso2(bottom \/ P, P).

/* cascara para el fPaso2 */
fPaso2Iterado(F, Rdo):-
	fPaso2(F, F2),
	F \= F2,
	fPaso2Iterado(F2, Rdo).
fPaso2Iterado(F, F):-
	fPaso2(F, F2),
	F = F2.

/* ELIMINO LOS PARENTESIS DE MÁS DE MI FBF */
eliminarParentesis(A, A) :- atom(A).
eliminarParentesis(~A, ~A) :- atom(A).
eliminarParentesis(A \/ D, Re) :- 
    D = B \/ C, 
    eliminarParentesis(A, Ra), 
    eliminarParentesis(B, Rb), 
    eliminarParentesis(C, Rc), 
    eliminarParentesis(Ra \/ Rb, Rd), 
    eliminarParentesis(Rd \/ Rc, Re).
eliminarParentesis(A \/ B, Ra \/ Rb) :- 
    eliminarParentesis(A, Ra), 
    eliminarParentesis(B, Rb).
eliminarParentesis(A /\ D, Re) :- 
    D = B /\ C, 
    eliminarParentesis(A, Ra), 
    eliminarParentesis(B, Rb), 
    eliminarParentesis(C, Rc), 
    eliminarParentesis(Ra /\ Rb, Rd), 
    eliminarParentesis(Rd /\ Rc, Re).
eliminarParentesis(A /\ B, Ra /\ Rb) :- 
    eliminarParentesis(A, Ra), 
    eliminarParentesis(B, Rb). 

/* GUARDAR UNA EXPRESION EN UNA LISTA */
guardarExpresion(A,[[A]]):-atomic(A).
guardarExpresion(~A,[[~A]]):-atomic(A).
guardarExpresion(A /\ B,L):-
    guardarExpresion(A,L1),
    guardarExpresion(B,L2),
    unir(L1,L2,L).
guardarExpresion(A \/ B,[L]):-
    guardarClausula(A\/B,L).

/* GUARDAR UNA CLAUSULA */
/* caso general donde se separa los \/ */
guardarClausula(A \/ B, L):-
    guardarClausula(A,L1),
    guardarClausula(B,L2),
    append(L1,L2,L).
/* Caso base donde no hay mas \/ */
guardarClausula(A,[A]).

/* UNIR DOS LISTAS */
unir(Lx,[],Lx).
unir([],Ly,Ly).
unir([X|Lx],L2,[X|Lrta]):-
    unir(Lx,L2,Lrta).

/* ELIMINAR ELEMENTOS REPETIDOS */
/* caso base */
eliminarRepetidos([X],[Lx]):- reducirClausula(X,Lx).
/* caso recursivo */
eliminarRepetidos([X|Lx],[LX|Lresto]):-
    eliminarRepetidos(Lx,Lresto),
    reducirClausula(X,LX).

/* BORRAR TODAS LAS APARICIONES DE UN ELEMENTO EN UNA LISTA */
borrarTodas(_,[],[]).
borrarTodas(X,[X|Xs],L):- borrarTodas(X,Xs,L).
borrarTodas(X,[T|Ts],[T|L1]):- borrarTodas(X,Ts,L1).


/* REDUCIR UNA CLAUSULA */
/* caso base */
reducirClausula([],[]).
/* caso recursivo */
reducirClausula([X|Lx],[X|Lrta]):-
    borrarTodas(X,Lx,Lrto),
    reducirClausula(Lrto,Lrta).

/* ENCONTRAR COMPLEMENTARIOS */
esta(X, [X | _]).
esta(X, [_ | T]) :- esta(X, T).

/* EXISTE COMPLEMENTARIO */
/* devuelve true si existe el complementario del elemento
   X en la lista, false en caso contrario */
existeComplementario(X,Lista):- atomic(X), esta(~X,Lista).
existeComplementario(~X,Lista):- atom(X), esta(X,Lista).

/* TIENE COMPLEMENTARIO */
/* devuelve true si algun elemento de la clausula tiene
   complementario */
tieneComplementario([]) :- false.
tieneComplementario([X|Xs]):- 
    existeComplementario(X,Xs).
tieneComplementario([X|Xs]):-
    not(existeComplementario(X,Xs)),
    tieneComplementario(Xs).

/* GENERAR TOPS*/
/* caso base */
generarTops([Lx],Rta):-
    tieneComplementario(Lx),
    Rta=[[top]];
    Rta=[Lx].
/* caso general */
generarTops([X| Lx],Ls):-
    generarTops(Lx,Lz),
    tieneComplementario(X),
    Ls=[[top] | Lz];
    generarTops(Lx,Lz),
    Ls=[X | Lz].

/* ELIMINAR CLAUSULAS TOP */
/* casos base */
eliminarTops([],[]).
/* caso general */
eliminarTops([X|Lx],Rta):-  
    X==[top],
    eliminarTops(Lx,Lxx),
    Rta=Lxx;
    eliminarTops(Lx,Lxx),
    Rta=[X|Lxx].

crearTop([],[top]).
crearTop(X,X).

/*Obtener primer elemento de una lista */
primerElemento([],[]).
primerElemento([X|_],X).

/*	Verifica si la lista es de un elemento de longitud	*/
tieneUnElemento([_L1|Lx]):-Lx==[].

/* controla si el elemento E es igual al unico elemento de la lista */
sonComplementarios(E,[X]):- X= ~E.
sonComplementarios(~E,[X]):- atomic(E),E=X.

/* tiene complementario */
tieneComplementario(E,[Y|Ys]):-
    tieneUnElemento(Y),
    sonComplementarios(E,Y);
    tieneComplementario(E,Ys).    

/* GENERA LOS BOTTOMS */
/* caso base */
generarBottoms([],[]).
/* caso general */
generarBottoms([X|Xs],Rta):-
    tieneUnElemento(X),
    primerElemento(X,E),
    tieneComplementario(E,Xs),
	borrarComplementario(E,Xs,Rtaa),
	generarBottoms([bottom|Rtaa], Rta1),
    Rta=Rta1;
    generarBottoms(Xs,Rta2),
    Rta=[X|Rta2].

/* busca y si encuentra el complementario lo elimina. y escribe bottom */
/* caso base */
borrarComplementario(_X,[],[]).
/* caso general */
borrarComplementario(X,[Y|Ys],Rta):-
    tieneUnElemento(Y),
    sonComplementarios(X,Y),
    Rta=[bottom|Ys];
    borrarComplementario(X,Ys,Rta1),
    Rta=[Y|Rta1].

/* ELIMINAR CLAUSULAS BOTTOM */
eliminarBottoms(Lista, Rta):-
    borrarTodas(bottom, Lista, Rta1),
    fixListaVacia(Rta1, Rta).

fixListaVacia([], [bottom]).
fixListaVacia([X|Xs],[X|Xs]).

/* ELIMINAR CLAUSULAS REPETIDAS */
/* true si dos listas tienen la misma longitud */
mismoLargoListas(L1,L2):-
    length(L1,Rta1),
    length(L2,Rta2),
    Rta1==Rta2.

/* devuelve true si dos elementos o dos listas son iguales, 
false caso contrario*/
listasIguales([],[]). 
listasIguales(X,X).   
listasIguales([],_).
listasIguales([X|Xs],Lista):-
    esta(X,Lista),
    listasIguales(Xs,Lista).

borrarClausula([],Lista,Lista).
borrarClausula(_,[],[]).
borrarClausula([X|Xs],[Y|Ys],Rta):-
    mismoLargoListas([X|Xs],Y),
    listasIguales([X|Xs],Y),
    borrarClausula([X|Xs],Ys,Rta1),
    Rta=Rta1;
    borrarClausula([X|Xs],Ys,Rta2),
    Rta=[Y|Rta2].  

/* borrar clausulas repetidas de una lista */
borrarClausulasRepetidas([],[]).
borrarClausulasRepetidas([X|Xs],Rta):-
    borrarClausula(X,Xs,Rta1),
    borrarClausulasRepetidas(Rta1,Rta2),
    Rta=[X|Rta2]. 

/* PASAR DE UNA LISTA A UNA FBF */
pasarClausula([],_).
pasarClausula([X],X).
pasarClausula([X|Xs],Rta):-
    pasarClausula(Xs,Rta1),
    Rta= X \/ Rta1.

pasarAfbf([],_).
pasarAfbf([top],top).
pasarAfbf([bottom],bottom).
pasarAfbf([X],Rta):- pasarClausula(X,Rta).
pasarAfbf([X|Xs],Rta):-
    pasarClausula(X,Rta1),
    pasarAfbf(Xs,Rta2),
    Rta=Rta1/\Rta2.

transformarExpresion(A,Rta):- 
    pasarAfbf(A,Rta1),
    eliminarParentesis(Rta1,Rta).

/------------------------REFUTABLE------------------------------------/

refutable(S):- guardarExpresion(S,R),!,refutar(R).


/* une nos listas sin repetir los elementos */
unirSinRepeticiones([],C1,C1).
unirSinRepeticiones([X|Xs],C1,Rta2):- not(pertenece(X,C1)), insertar(X,C1,Rta), unirSinRepeticiones(Xs,Rta,Rta2).
unirSinRepeticiones([X|Xs],C1,Rta):- pertenece(X,C1), unirSinRepeticiones(Xs,C1,Rta).

/* insertar el elemento E en la lista,si es que E no pertenece a la lista */
insertar(E,[],[E]).
insertar(E,Conjunto,[E|Conjunto]):- not(pertenece(E,Conjunto)). 
insertar(E,Conjunto,Conjunto):- pertenece(E,Conjunto).

/* true si E pertenece a la lista, falso caso contrario */
pertenece(E,[X|_]):- E=X.
pertenece(E,[_|Xs]):- pertenece(E,Xs).


/-------------------PROGRAMA PRINCIPAL--------------------------------/


fncr(FBF,FNCR):-
    fBienEscrita(FBF,R1),
    writeln("Fbf sin implicaciones ni equivalencias"= R1),
    fPaso1(R1,R2),
    writeln("Fbf con negaciones acomodadas,Paso1"= R2),
    fPaso2Iterado(R2,FNC),
    writeln("Fbf despues del paso 2 "= FNC),
    eliminarParentesis(FNC,RTA),
    writeln("Fbf sin parentesis de mas "= RTA),
	guardarExpresion(RTA,RTA1),
    writeln("Fbf guardada en la lista "= RTA1),
    eliminarRepetidos(RTA1,RTA2),
    writeln("Fbf guardada en la lista sin repeticiones "= RTA2),
    generarTops(RTA2,RTA3),
    writeln("Fbf guardada en la lista luego de generar tops "= RTA3),
    eliminarTops(RTA3,RTA44),
    crearTop(RTA44,RTA4),
    writeln("Fbf guardada en la lista luego de borrar tops "= RTA4),
	generarBottoms(RTA4,RTA5),    
	writeln("Fbf guardada en la lista luego de generar bottoms "= RTA5),
    eliminarBottoms(RTA5,RTA6),
    writeln("Fbf guardada en la lista luego de borrar bottoms "= RTA6),
    borrarClausulasRepetidas(RTA6,RTA7),
    writeln("Fbf guardada en la lista luego de borrar clausulas repetidas "= RTA7),
    transformarExpresion(RTA7,FNCR).
