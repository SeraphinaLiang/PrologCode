link(a,b).
link(b,a).
link(b,c).
link(c,b).
link(b,d).
link(d,b).
link(c,d).
link(d,c).

/*neighbor*/
neighbor(X,Y):-link(X,Y),link(Y,X).

/*path which will never end in circles*/
path(X,Y):-link(X,Y);link(X,T),path(T,Y),X\==Y.

/*path which avoid circles*/
path2(X,Y):-path2Helper(X,Y,[X]).
path2Helper(X,Y,V):-link(X,Y),\+(member(Y,V));
                    link(X,T),\+(member(T,V)),path2Helper(T,Y,[T|V]).
