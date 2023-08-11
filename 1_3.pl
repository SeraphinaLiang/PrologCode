and(B1,B2) :- B1=tru,B2=tru.
or(B1,B2) :- B1=tru ; B2=tru.
not(B) :- B =fal.
eval(K,M) :- K=tru,M=tru;
             K=fal,M=fal;
             % evaluate K to tru/fal
             K,M=tru;
             \+K,M=fal.
