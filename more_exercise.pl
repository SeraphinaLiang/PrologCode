% minmaxtree(node(1-node(2-node(1-leaf(who), 3-leaf(knew)),
% 4-node(0-leaf(prolog), 1-leaf(was))), 3-node(4-node(5-leaf(such),
% 2-leaf(fun)), 6-node(2-leaf(!), 4-leaf(!)))), Result).

minmaxtree(T,R):- minmaxtree_min(T,R).

% node(C1-node(T1),C2-node(T2)) ----> WRONG

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
