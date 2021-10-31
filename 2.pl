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
    fib2(N,F,0,1,3).
fib2(N,F,F1,F2,N):-
    F is F1 + F2.
fib2(N,F,F1,F2,M):-
    Temp is F1 + F2,
    Count is M + 1, 
    fib2(N,F,F2,Temp,Count).

