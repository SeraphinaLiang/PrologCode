and(B1,B2) :- B1=tru,B2=tru.
or(B1,B2) :- B1=tru ; B2=tru.
not(B) :- B =fal.
eval(K,M) :- K=tru,M=tru;
             K=fal,M=fal;
             K=tru,M;
             K,M=tru;
             K=fal,\+M;
             \+K,M=fal.