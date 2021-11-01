% ----------------------- graph -----------------------

link(a,b).
link(b,d).
link(b,c).
link(c,d).

neighbor(X,Y):-link(X,Y);link(Y,X).
path(X,Y):- neighbor(X,Y).
path(X,Y):-path(X,Z),neighbor(Z,Y),X\==Y.

path2(X,Y) :- pathHelper(X,Y,[X]).
pathHelper(X,Y,L):- neighbor(X,Y),\+member(Y,L).
pathHelper(X,Y,L):- 
    neighbor(X,Z),
    \+member(Z,L),
    pathHelper(Z,Y,[Z|L]).

% ----------------------- fibonacci -----------------------

fib(1,0).
fib(2,1).
fib(N,F):-
    N1 is N-1,
    N2 is N-2,
    fib(N1,A),
    fib(N2,B),
    F is A+B.

fib2(1,0).
fib2(2,1).
fib2(N,F):- 
    N > 2, 
    fib2(N,F,0,1,3).
fib2(N,F,F1,F2,N):-
    F is F1 + F2.
fib2(N,F,F1,F2,M):-
    Temp is F1 + F2,
    Count is M + 1, 
    fib2(N,F,F2,Temp,Count).

% ----------------------- calculator -----------------------

% from "Boolean Formulas"
eval(tru,tru).
eval(fal,fal).

eval(and(E1,E2),tru) :-  eval(E1,tru),eval(E2,tru).
eval(and(E1,E2),fal) :-  eval(E1,fal) ; eval(E2,fal).

eval(or(E1,E2),tru) :- eval(E1,tru);eval(E2,tru).
eval(or(E1,E2),fal) :- eval(E1,fal),eval(E2,fal).

eval(not(E1),fal) :- eval(E1,tru).
eval(not(E1),tru) :- eval(E1,fal).


eval(number(E),E).

eval(plus(E1,E2),C):-
    eval(E1,V1),
    eval(E2,V2),
    C is V1 + V2.

eval(min(E1,E2),V):-
    eval(E1,V1),
    eval(E2,V2),
    V is V1-V2.

eval(neg(E),V):-
    eval(E,EV),
    V is -EV.

eval(=(E1,E2), tru):-
    eval(E1,V1),
    eval(E2,V2),
    V1 =:= V2.

eval(=(E1,E2), fal):-
    eval(E1,V1),
    eval(E2,V2),
    V1 =\= V2.
   

% ----------------------- expressive -----------------------

eval(int(I),_,I).

eval(var(A),[pair(A,N)|_],N).
eval(var(A),[_|L],Value):-
    eval(var(A),L,Value).

% ---method 2 for var(x)---
%eval(var(A),[pair(X,N)|L],Value):-
%    A=X,Value=N;
%    eval(var(A),L,Value).

eval(plus(E1,E2),List,Value):-
    eval(E1,List,Value1),
    eval(E2,List,Value2),
    Value is Value1 + Value2.

eval(times(E1,E2),List,Value):-
    eval(E1,List,Value1),
    eval(E2,List,Value2),
    Value is Value1 * Value2.

eval(min(E),List,Value):-
    eval(E,List,Value1),
    Value is -Value1.

eval(pow(E1,E2),List,Value):-
    eval(E1,List,Value1),
    eval(E2,List,Value2),
    Value is Value1 ** Value2.
    
    
% ----------------------- prime numbers -----------------------


