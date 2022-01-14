/*********************************************************************************************
  SOLUTION for exercise 'Holiday lights'

  tour/1 computes a Hamiltonian path beginning and ending in node 1 of a given graph, 
  making sure that all edges are used and that no two adjacent edges in the Hamiltonian 
  path have the same label.

  PREREQUISITE:
    - edges of graphs are given with highway/3 facts

**********************************************************************************************/

highway(1,2,yellow).
highway(2,3,blue).
highway(1,3,yellow).

%% Alternative highway.
%% COMMENT OUT the highway/3 facts above BEFORE uncommenting these:
%%
%% highway(1,2,c).
%% highway(2,3,a).
%% highway(1,3,b).
%% highway(3,5,a).
%% highway(3,4,c).
%% highway(5,4,d).


node(X) :- highway(X,_,_).
node(X) :- highway(_,X,_).

neighbor(Node,Neighbor) :- highway(Node,Neighbor,_).
neighbor(Node,Neighbor) :- highway(Neighbor,Node,_).

color(Node,Color) :- highway(Node,_,Color).
color(Node,Color) :- highway(_,Node,Color).


% allRoads/1: returns all the roads (edges) in the graph
allRoads(Roads) :- 
  findall(highway(X,Y,T),highway(X,Y,T),UnsortedRoads),
  sort(UnsortedRoads,Roads).

% degree/2: compute the degree of a node (the number of connections).
degree(Node,N) :-
  findall(Neighbor,neighbor(Node,Neighbor),L),
  length(L,N).

% check/0: checks the first constraint and the second constraint
check :-
  \+ (node(X), (\+ check_even_at(X) ; \+ check_colors_at(X))).

% check_even_at/1: checks whether a node has an even degree
check_even_at(Node) :-
  degree(Node,N),
  0 is N mod 2.

% check_colors_at/1: check whether a node =/= 1 which has at N connections lit by
% some colour C has at least N connections lit by other colours.
check_colors_at(Node) :- Node = 1.
check_colors_at(Node) :-
  degree(Node,N),
  \+ (
    color(Node,Color),
    findall(
      Color,
      (highway(Node,_,Color);highway(_,Node,Color)),
      ColorConnections),
    length(ColorConnections,NColor),
    NColor > N / 2).

% tour/1: returns the smallest tour
tour(T) :-
  check,
  findall(Tour,tourcandidate(Tour),L),
  sort(L,[T|_OtherTours]).

% tourcandidate/1: computes a possible tour
tourcandidate(Tour) :-
  allRoads(Roads),
  tour(1,1,Roads,[],TourRev),
  reverse(TourRev,Tour).

% tour(Begin,End,RoadsLeft,VisitedRoads,Solution)
tour(End,End,[],Visited,Visited). % stop when there are no edges left
tour(Node,End,RoadsLeft,Visited,Solution) :-
  ( 
    select(highway(Node,Neighbor,Type),RoadsLeft,RoadsLeft1)
  ; 
    select(highway(Neighbor,Node,Type),RoadsLeft,RoadsLeft1)
  ),
  compatible(Type,Visited),
  tour(Neighbor,End,RoadsLeft1,[Neighbor-Type|Visited],Solution).
	
% compatible/2: checks whether a road can be added to the tour by checking its type
compatible(_,[]).
compatible(Type,[ _-Type2|_]) :- Type \== Type2.
