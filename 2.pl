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
    fib2(N,0,1,F,3).
fib2(N,Value1,Value2,Value,N):-
    Value is Value1 + Value2.
fib2(N,Value1,Value2,Value,Count):-
    Temp is Value2 + Value1,
    Cnt1 is Count + 1, 
    fib2(N,Value2,Temp, Value, Cnt1).
