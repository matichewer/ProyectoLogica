:- op(300,fx,~). % negaci´on, prefija, no asociativa.
:- op(400,yfx,(/\)). % conjunci´on, infija, asociativa a izquierda.
:- op(500,yfx,(\/)). % disyunci´on, infija, asociativa a izquierda.
:- op(600,xfx,=>). % implicaci´on, infija, no asociativa.
:- op(650,xfx,<=>). % equivalencia, infija, no asociativa.

/*
teorema(F):- fncr(F,FNCR), refutable(FNCR).
fncr(F, FNCR ).
refutable(F).
*/

transImplica( P => Q, ~(P) \/ (Q) ).
/*
?- transImplica(a => (b => c), Rdo).
Rdo = ~a \/ (b => c)
*/
transEquivalencia( P <=> Q, (~(P)\/(Q)) /\ ((P) \/ ~(Q)) ).

/*RTA no tiene el simbolo de iguales ni el de implicacion*/
fncr(F,FNCR):- fBienEscrita(F,RTA), 
/*
posible solucion, falta implementar las dos sin
fBienEscrita(F,RTA):-  fSinEquivalencias(F,Rta1), fSinImplicaciones(Rta1,RTA).
fSinEquivalencias(F,RTA).
fSinImplicaciones(F,RTA).
*/
