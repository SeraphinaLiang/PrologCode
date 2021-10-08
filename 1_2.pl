s(zero).
peano_plus(zero,X,X).
peano_plus(s(X),Y,s(Z)) :- peano_plus(X,Y,Z).

min(X,X,zero).
min(s(X),Y,s(Z)) :- min(X,Y,Z).

greater_than(X,Y) :- min(X,Y,Z),Z \=zero.
maximum(X,Y,Z) :- greater_than(X,Y), Z=X; greater_than(Y,X),Z=Y;Z=X.
