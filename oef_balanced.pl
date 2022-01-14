% A tree is balanced if the depths of the left and right subtree differ by at
% most one, and both subtrees are balanced.

max_one_difference(A,A).
max_one_difference(A,B) :-
    A is B + 1.
max_one_difference(A,B) :-
    B is A + 1.

depth(nil,0).
depth(t(L,_,R),D) :-
    depth(L,LD),
    depth(R,RD),
    D1 is max(LD,RD),
    D is D1 + 1.

balanced(nil).
balanced(t(Left,_,Right)) :-
    depth(Left,DepthLeft),
    depth(Right,DepthRight),
    max_one_difference(DepthLeft,DepthRight),
    balanced(Left),
    balanced(Right).

% The implementation of add_to/3, which adds an element E to a tree B such that
% the resulting tree is still balanced. The given tree B should be balanced.
% By inserting an element in the shallowest subtree, you do not have to verify
% the balancedness of the tree.

% add_to/3
add_to(nil,E,t(nil,E,nil)).
add_to(t(L,W,R),E,t(NL,W,NR)) :-
    depth(L,LD),
    depth(R,RD),
    add_to(L,R,E,LD,RD,NL,NR).

% add_to/7
add_to(L,R,E,LD,RD,L,NR) :-
    LD > RD,
    add_to(R,E,NR).
add_to(L,R,E,LD,RD,NL,R) :-
    LD =< RD,
    add_to(L,E,NL).

% If both subtrees are equally deep, we pick the left subtree.

% New tree representation: store in each t both a value and the depth.
% t(LeftSubtree,Value,RightSubtree,Depth)

% The implementation of diepte2/2 in constant time.
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

