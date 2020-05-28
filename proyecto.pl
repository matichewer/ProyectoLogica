:- op(300,fx,~). % negacion, prefija, no asociativa.
:- op(400,yfx,(/\)). % conjuncion, infija, asociativa a izquierda.
:- op(500,yfx,(\/)). % disyuncion, infija, asociativa a izquierda.
:- op(600,xfx,=>). % implicacion, infija, no asociativa.
:- op(650,xfx,<=>). % equivalencia, infija, no asociativa.

/*
teorema(F):- fncr(F,FNCR), refutable(FNCR).
fncr(F, FNCR ).
refutable(F).
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

transEquivalencias( P <=> Q, (~(P1)\/(Q1)) /\ ((P1) \/ ~(Q1)) ):-
    transEquivalencias(P,P1),
    transEquivalencias(Q,Q1).
transEquivalencias(P,P).

/*funciona recursivamente
?- transImplicaciones((a => c) => b, R).
R = ~(~a\/c)\/b
funciona recursivamente
?- transEquivalencias((p<=>k) <=> q,R).
R = (~((~p\/k)/\(p\/~k))\/q)/\((~p\/k)/\(p\/~k)\/~q)
*/

/*RTA no tiene equivalencias ni impliaciones*/
fBienEscrita(F,RTA):- 
    transImplicaciones(F,Rta1),
    transEquivalencias(Rta1,RTA).	
 /*
?-fBienEscrita( ( (a => b) <=> c ) , R ).
(~(a=>b)\/c)/\((a=>b)\/~c)
ERROR no me resuelve las implicaciones ERROR ????????????????????
*/

fPaso1a( ~(~F) , (F1) ):- fPaso1a(F,F1).
fPaso1a(F,F).
/*funciona recursivamente
?- fPaso1a(~(~(~(~(~(~a))))), R).
R=a.
*/

fPaso1b( ~( (P) \/ (Q) ) , ~(P1) /\ ~(Q1) ):-
    fPaso1b(P,P1),
    fPaso1b(Q,Q1).
fPaso1b(F,F).
/*funciona recursivamente
?- fPaso1b(~((~(b \/ c )) \/ (a)),R).
R = ~(~b/\~c)/\~a
*/

fPaso1c( ~((P)/\(Q)) , ~(P1) \/ ~(Q1) ):-
    fPaso1c(P,P1),
    fPaso1c(Q,Q1).
fPaso1c(F,F).
/*funciona recursivamente
?- fPaso1c( ~(~(b/\ c) /\ a),R).
R = ~(~b\/~c)\/~a
*/

fPaso1d( ~(bottom) , top ).
fPaso1d(F,F).
fPaso1e( ~(top) , bottom ).
fPaso1e(F,F).

/*RTA me devuelve a F luego de aplicarle los pasos a,b,c,d,e*/
fPasoUno(F,RTA):- 
    fPaso1a(F, Rta1),
    fPaso1b(Rta1, Rta2),
    fPaso1c(Rta2,Rta3),
    fPaso1d(Rta3,Rta4),
    fPaso1e(Rta4,RTA).
/*
?- fPasoUno(~(~(~a) \/ b), R).
R = ~(~(~a))/\~b 
ERROR NO ME RESUELVE LA NEGACION !!!!!!!!!!!!!!!!!
*/

fPaso2a( (P1) \/ ( P2 /\ P3 ), ( ((P11) \/ (P22)) /\ ((P11) \/ (P33)) ) ):-
    fPaso2a(P1,P11),
    fPaso2a(P2,P22),
    fPaso2a(P3,P33).
fPaso2a(P,P).
/* funciona recursivamente
?- fPaso2a( (a \/ (b /\ c) \/ (d /\ e) )  , R).
R = ((a\/b)/\(a\/c)\/d)/\((a\/b)/\(a\/c)\/e)
*/

/* MAL SE HACE CON LISTA SEGUN EL PROFE EN EL FORO
 * PARA NO CAER EN UN BUCLE
fPaso2b( (P1) \/ (P2) , ( (P2) \/ (P1) ) ).
fPaso2bb( (P1) /\ (P2) , ( (P2) /\ (P1) ) ).
*/
/*
fPaso2c(_P \/ top, _P ).
fPaso2c(P,P).
fPaso2d(_P \/ bottom, _P).
fPaso2d(P,P).*/

/*RTA me devuelve a F luego de aplicarle los pasos a,b,c y d
fPasoDos(F, RTA):-
    fPaso2a(F,Rta1),
    fPaso2b(Rta1,Rta2),
    fPaso2c(Rta2,Rta3),
    fPaso2d(Rta3,RTA).
*/

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
