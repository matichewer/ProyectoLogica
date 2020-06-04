:- op(300,fx,~). % negacion, prefija, no asociativa.
:- op(400,yfx,(/\)). % conjuncion, infija, asociativa a izquierda.
:- op(500,yfx,(\/)). % disyuncion, infija, asociativa a izquierda.
:- op(600,xfx,=>). % implicacion, infija, no asociativa.
:- op(650,xfx,<=>). % equivalencia, infija, no asociativa.

/* 
teorema(F):- fncr(F,FNCR), refutable(FNCR). 
*/


/* ELIMINO TODAS LAS IMPLICACIONES DE MI FBF */

transformarImplicaciones(P,P):- atomic(P).
transformarImplicaciones(~P,~(RTA)):-transformarImplicaciones(P,RTA).
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
transformarEquivalencias(~P,~(RTA)):- transformarEquivalencias(P,RTA). 
transformarEquivalencias( P <=> Q, (~(P1)\/(Q1)) /\ (~(Q1) \/ (P1)) ):-
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


/*-------------------TRANSFORMAR A FNCR--------------------------------*/

/* PRIMER PASO PARA TRANSFORMAR A FNCR */

fPaso1(P,P):- atomic(P).
fPaso1( ~P, ~P):- atomic(P).
fPaso1(~top, bottom).
fPaso1(~bottom, top).
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

/* Cascara para el fPaso2 */
fPaso2Iterado(F, Rdo):-
	fPaso2(F, F2),
	F \= F2,
	fPaso2Iterado(F2, Rdo).
fPaso2Iterado(F, F):-
	fPaso2(F, F2),
	F = F2.
	
/* TERCER PASO PARA TRANSFORMAR A FNCR */

fPaso3(P,P):-atomic(P).
fPaso3(~P,~P):- atomic(P).
/*bottom*/
fPaso3(bottom,bottom).
fPaso3( _ /\ bottom, bottom).
fPaso3(bottom /\ _ , bottom).
fPaso3(P /\ Q, P1 /\ Q1 ):-
    fPaso3(P,P1),
    fPaso3(Q,Q1).
/*top*/
fPaso3(top,top). 
fPaso3(top /\ P, P1):- fPaso3(P,P1).
fPaso3a(P /\ top, P1):-fPaso3a(P,P1).
