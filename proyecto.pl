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

transImplicaciones( P => Q, ~(P1) \/ (Q1) ):- 
    transImplicaciones(P,P1),
    transImplicaciones(Q,Q1).
transImplicaciones(P,P).

transEquivalencias( P <=> Q, (~(P1)\/(Q1)) /\ ((P1) \/ ~(Q1)) ):-
    transEquivalencias(P,P1),
    transEquivalencias(Q,Q1).
transEquivalencias(P,P).

/*
?- transImplicaciones((a => c) => b, R).
R = ~(~a\/c)\/b
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
/*
?- fPaso1a(~(~(~(~(~(~a))))), R).
R=a.
*/

fPaso1b( ~( (P) \/ (Q) ) , ~(P1) /\ ~(Q1) ):-
    fPaso1b(P,P1),
    fPaso1b(Q,Q1).
fPaso1b(F,F).
/*
?- fPaso1b(~((~(b \/ c )) \/ (a)),R).
R = ~(~b/\~c)/\~a
*/

fPaso1c( ~((P)/\(Q)) , ~(P1) \/ ~(Q1) ):-
    fPaso1c(P,P1),
    fPaso1c(Q,Q1).
fPaso1c(F,F).
/*
?- fPaso1c( ~(~(b/\ c) /\ a),R).
R = ~(~b\/~c)\/~a
*/

fPaso1d( ~(bottom) , top ).
fPaso1d(F,F).
fPaso1e( ~(top) , bottom ).
fPaso1e(F,F).



/*fncr(F,FNCR):- fBienEscrita(F,RTA),*/
