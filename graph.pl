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
    
% -------------finding a spanning tree of a graph in spread way--------------

stree(Graph,Tree):-
    member(Edge,Graph),
    spread([Edge],Tree,Graph).

% spread(Tree1,Tree,Graph) : Tree1 spread to Tree
spread(Tree1,Tree,Graph):-
    addEdge(Tree1,Tree2,Graph),
    spread(Tree2,Tree,Graph).
spread(Tree,Tree,Graph):-
    \+addEdge(Tree,_,Graph). % no edge can be add any more, without creating a cycle

% addEdge(Tree,NewTree,Graph)
addEdge(Tree,[A-B|Tree],Graph):-
    adjacent(A,B,Graph),
    node(A,Tree),
    \+node(B,Tree).

% Graph = [a-b,b-c,b-d,c-d]
adjacent(N1,N2,Graph):-
    member(N1-N2,Graph);
    member(N2-N1,Graph).

node(N,Graph):-
    adjacent(N,_,Graph). % node is a node in graph, if node is adjacent to anything in graph

% -------------finding a spanning tree of a graph in definition way--------------
% 1. T is a spanning tree of G if:
%    - T is a subset of G
%    - T is a Tree
%    - each node in G is also in T
% 2. T is a tree if:
%    - T is connected
%    - T has no cycle

stree(Graph,Tree):-
    subset(Graph,Tree),
    tree(Tree),
    covers(Tree,Graph).

tree(T):-
    connected(T),
    \+hasacycle(T).

connected(G):-
    \+(node(A,G),node(B,G),\+path(A,B,G,_)).

hasacycle(G):-
    adjacent(N1,N2,G),
    path(N1,N2,G,[N1,X,Y|_]). % path length > 1

% covers(Tree,Graph) : every node in Graph is in Tree
covers(Tree,Graph):-
    \+(node(N,Graph),\+node(N,Tree)).

% subset(Set,Subset)
subset([],[]).
subset([X|L1],[X|L2]):-
    subset(L1,L2).
subset([_X|L1],L2):-
    subset(L1,L2).
    


