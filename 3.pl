--------------balanced trees---------------
balanced(nil).
balanced(t(nil,_,nil)).
balanced(t(LTree,_,RTree)):-
    depth(LTree,DL),
    depth(RTree,DR),
    S is DL-DR,
    one(S),
    balanced(LTree),
    balanced(RTree).

%  abs(DL - DR) <= 1 ?
one(S):-
    S =:= 1;
    S =:= -1;
    S =:= 0.

depth(t(nil,_,nil),1).
depth(nil,0).
depth(t(L,_,R),Depth):-
    depth(L,DL),
    depth(R,DR),
    max(DL,DR,Max),
    Depth is Max+1.

max(A,B,A):- A>B.
max(_,B,B).

% randomly add Elem to the Tree, and then check whether the Tree is balanced.

add_to(nil,Elem,t(nil,Elem,nil)).

add_to(t(L,V,R),Elem,t(L,V,NewTreeRight)):-
    add_to(R,Elem,NewTreeRight),
    balanced(t(L,V,NewTreeRight)).
    
add_to(t(L,V,R),Elem,t(NewTreeLeft,V,R)):-
    add_to(L,Elem,NewTreeLeft),
    balanced(t(NewTreeLeft,V,R)).
    
% use new representation t(L,V,R,Depth)
% New tree representation: store in each t both a value and the depth.
% t(LeftSubtree,Value,RightSubtree,Depth)

% The implementation of depth2/2 in constant time.
depth2(nil,0).
depth2(t(_,_,_,D),D).

add_to2(nil,E,t(nil,E,nil,1)).
add_to2(t(L,W,R,_),E,t(L,W,NR,ND)) :-
   depth2(L,LD),
   depth2(R,RD),
   LD > RD,
   add_to2(R,E,NR),
   depth2(NR,NRD),
   ND is max(NRD,LD) + 1.
add_to2(t(L,W,R,_),E,t(NL,W,R,ND)) :-
   depth2(L,LD),
   depth2(R,RD),
   LD =< RD,
   add_to2(L,E,NL),
   depth2(NL,NLD),
   ND is max(NLD,RD) + 1.

-------------alpha-beta pruning-----------

alpha_beta(leaf(S,V),_,_,S,leaf(S,V)).

alpha_beta(max(L,R),Alpha,Beta,S,max(NewL,NewR)) :-
    alpha_beta(L,Alpha,Beta,ScoreL,NewL),
    NewAlpha is max(ScoreL,Alpha),
    (
	Beta =< NewAlpha   % if
     ->
	  S = ScoreL,      %then
	  NewR = nil
     ;
     alpha_beta(R,NewAlpha,Beta,ScoreR,NewR),  %else
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
     
% --------test-------
alpha_beta(max(
               min(
                   max(leaf(1,who),leaf(3,know)),
                   max(leaf(5,prolog),leaf(1,was))),
               min(
                   max(leaf(2,such),leaf(2,fun)),
                   max(leaf(5,!),leaf(4,!)))),
           -10,10,S,T).
	   
-------------------n queens--------------------
% queens(5,solutionList).

queens(N,L):-
    numlist(1,N,List),  %build-in: generate list[1..N]
    permutation(List,L),
    safe(L).

permutation([],[]).
permutation(List,[Head|Tail]):-  % and add it back in order
    del(Head,List,List1),  % randomly delete an item each time
    permutation(List1,Tail).

del(Item,[Item|List],List).  % the item deleted is at 1st position
del(Item,[Other|List],[Other|List1]):-   % item deleted is in the tail
    del(Item,List,List1).

safe([]). 
safe([Queen|Other]):-
    safe(Other),  % other queens do not attack each other
    noattack(Queen,Other,1).  % the queen do not attack other queens

noattack(_,[],_).  % Other is empty list --> no attack
noattack(Y,[Y1|Ylist],Xdist):-   
    Y1-Y =\= Xdist,   % abs(Y1-Y2) != abs(X1-X2) --> different diagonal
    Y-Y1 =\= Xdist,
    Dist1 is Xdist + 1,
    noattack(Y,Ylist,Dist1).
  
-----------------------------------
