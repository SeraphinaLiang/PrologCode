% minmaxtree(node(1-node(2-node(1-leaf(who), 3-leaf(knew)),
% 4-node(0-leaf(prolog), 1-leaf(was))), 3-node(4-node(5-leaf(such),
% 2-leaf(fun)), 6-node(2-leaf(!), 4-leaf(!)))), Result).
% Result = prolog

minmaxtree(T,R):- minmaxtree_min(T,R).

% node(C1-node(T1),C2-node(T2)) ----> WRONG, cant unify, !! cannot mix terms and variables !!

minmaxtree_max(leaf(R),R).
minmaxtree_max(node(C1-T1,C2-T2),R):-
    C1 >= C2,
    minmaxtree_min(T1,R);
    minmaxtree_min(T2,R).

minmaxtree_min(leaf(R),R).
minmaxtree_min(node(C1-T1,C2-T2),R):-
    C1 >= C2,
    minmaxtree_max(T2,R);
    minmaxtree_max(T1,R).

-------------------------------------------------------------------
teaches(holvoet,bvp).
teaches(moens,bvp).
teaches(demoen,fi).
teaches(dedecker,socs).
teaches(holvoet,swop).
teaches(jacobs,swop).
teaches(demoen,ai).
teaches(deschreye,ai).

takes(joachim,bvp).
takes(joachim,fi).
takes(joachim,ai).
takes(matthias,bvp).
takes(matthias,ai).
takes(thomas,socs).
takes(thomas,swop).
takes(ingmar,swop).

takes_same_course(X,Y):-
    findall(pair(X1,Y1),(takes(X1,A),takes(Y1,A)),L),
    sort(L,LS),
    member(pair(X,Y),LS),
    X \== Y.

teach_same_course(X,Y):-
    findall(pair(X1,Y1),(teaches(X1,A),teaches(Y1,A)),L),
    list_to_set(L,LS),
    member(pair(X,Y),LS),
    X \== Y.

teaches_multiple_courses(Y):-
    findall(F,teaches(F,_),L),
    sort(L,SY),
    include(more_than_one,SY,SSY),
    member(Y,SSY).

more_than_one(I):-
    findall(teaches(I,A),teaches(I,A),L),
    length(L,LEN),
    LEN > 1.
