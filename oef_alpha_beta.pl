alpha_beta(leaf(S,V),_,_,S,leaf(S,V)).
alpha_beta(max(L,R),Alpha,Beta,S,max(NewL,NewR)) :-
    alpha_beta(L,Alpha,Beta,ScoreL,NewL),
    NewAlpha is max(ScoreL,Alpha),
    (
	Beta =< NewAlpha
     ->
	  S = ScoreL,
	  NewR = nil
     ;
         alpha_beta(R,NewAlpha,Beta,ScoreR,NewR),
	 S is max(ScoreL,ScoreR)
    ).
alpha_beta(min(L,R),Alpha,Beta,S,min(NewL,NewR)) :-
    alpha_beta(L,Alpha,Beta,ScoreL,NewL),
    NewBeta is min(ScoreL,Beta),
    (
	NewBeta =< Alpha
     ->
	 S = ScoreL,
	 NewR = nil
     ;
         alpha_beta(R,Alpha,NewBeta,ScoreR,NewR),
	 S is min(ScoreL,ScoreR)
    ).
     
