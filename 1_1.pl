sibling(X,Y) :- father(F,X),father(F,Y),mother(M,X),mother(M,Y),X\==Y.

father(anton,bart).
father(anton,daan).
father(anton,elisa).
father(fabian,anton).
mother(celine,bart).
mother(celine,daan).
mother(celine,gerda).
mother(gerda,hendrik).