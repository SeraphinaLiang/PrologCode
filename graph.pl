% graph typical question
% 1. find a path between 2 given nodes
% 2. find a subgraph e.g spanning tree

% ------------finding a path-----------
% path(A,Z,Graph,Path): Path is an acyclic path from A to Z in Graph

path(A,Z,Graph,Path):-
    path1(A,[Z],Graph,Path).

path1(A,[A|Path1],_,[A|Path1]). % end condition
path1(A,[Y|Path1],Graph,Path):-
    adjacent(X,Y,Graph),        % randomly find a X
    \+member(X,Path1),          % no cycle
    path1(A,[X,Y|Path1],Graph,Path).

% ------------finding a path with cost-----------

path(A,Z,Graph,Path,Cost):-
    path1(A,[Z],0,Graph,Path,Cost).

path1(A,[A|Path1],Cost1,_Graph,[A|Path1],Cost1). % end case is the final condition instead of initial one
path1(A,[Y|Path1],Cost1,Graph,Path,Cost):-
    adjacent(X,Y,CostXY,Graph),
    \+member(X,Path1),
    Cost2 is Cost1 + CostXY,
    path1(A,[X,Y|Path1],Cost2,Graph,Path,Cost).
    

