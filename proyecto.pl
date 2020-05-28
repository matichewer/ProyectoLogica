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
transformarImplicaciones( P => Q, ~(P1) \/ (Q1) ):- 
    transformarImplicaciones(P,P1),
    transformarImplicaciones(Q,Q1).
transformarImplicaciones(P \/ Q, (P1) \/ (Q1) ):-
    transformarImplicaciones(P,P1),
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
transformarEquivalencias( P <=> Q, (~(P1)\/(Q1)) /\ ((P1) \/ ~(Q1)) ):-
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
fPaso1( ~(~P) , (P1) ):- fPaso1(P,P1).
fPaso1( ~(~P) , (P) ):- atomic(P).
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



fPaso2( (P1) \/ ( P2 /\ P3 ), ( ((P11) \/ (P22)) /\ ((P11) \/ (P33)) ) ):-
    fPaso2(P1,P11),
    fPaso2(P2,P22),
    fPaso2(P3,P33).
fPaso2a(P,P).

fPaso2(_P \/ top, top ).
fPaso2(top \/ _P, top ).
fPaso2(P \/ bottom, P).
fPaso2(bottom \/ P, P).

/* TERCER PASO PARA TRANSFORMAR A FNCR */

fPaso3a(P /\ top, P1):-fPaso3a(P,P1).
fPaso3a(P,P).
/* funciona recursivamente
fPaso3a( (a /\ top) /\ top, R).
R = a
*/

fPaso3b( _ /\ bottom, bottom).
fPaso3b(P,P).
/* funciona creeeo
fPaso3b( (a /\ top) /\ bottom , R).
R = bottom
*/

/*RTA me devuelve a F luego de aplicarle los pasos a y b*/
fPasoTres(P,RTA):-
    fPaso3a(P,Rta1),
    fPaso3b(Rta1,RTA).
