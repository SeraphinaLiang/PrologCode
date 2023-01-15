https://swish.swi-prolog.org/p/compress_exam.pl

code([a,b], 1).
code([b,e], 2).
code([b,c,d,e], 3).
code([b,c], 4).
code([c,d],5).
code([a,b,c],6).

decompress([],[]).
decompress([X|Compressed],List):-
    uncode(X,XL),
    append(XL,Decompressed,List),
    decompress(Compressed,Decompressed).

uncode(X,XL):-
    code(XL,X).
uncode(X,[X]).

compress(X,R):-
    % L= [[],[],...]
    findall(Y,compress1(X,Y),L),
    sel(L,L1),
    select(R,L1,_).

sel([],[]).
sel([X|L],[X|R]):-
    check(X),
    sel(L,R).
sel([X|L],R):-
    \+check(X),!,
    sel(L,R).

check(Compress):-
    findall(R,decompress(R,Compress),L),
    length(L,LEN),!,
    LEN =:=1.
    
compress1([],[]).
compress1(UN,[Y|COM]):-
    code(X,Y),
    prefix(X,UN),
    length(X,LEN),
    LEN >=1,!,    % if no match
    strip(X,UN,NEW),
    compress1(NEW,COM).
compress1(UN,NEWCOM):-
    prefix(X,UN),
    length(X,LEN),
    LEN =:=1,
 %   LEN <2,
    !,
    strip(X,UN,NEW),
    append(X,COM,NEWCOM),
    compress1(NEW,COM).

strip([],L,L).
strip([A|K],[A|L],N):-
    strip(K,L,N).


optcompression(Un,Com):-
    findall(C,compress(Un,C),LC),
    find_len(LC,ListLen),
    min_list(ListLen,Min),!,
    member(Com,LC),
    length(Com,Min).

find_len([],[]).
find_len([X|List],[N|Len]):-
    length(X,N),
    find_len(List,Len).





-------------solutions-------------


decompress(Compressed, Decompressed):-
	decompress(Compressed, [], Decompressed).

decompress([], Acc, Acc).

decompress([C|T], Acc, Decompressed):-
	code(Orig, C),
	append(Acc,Orig, NewAcc),
	decompress(T, NewAcc, Decompressed).

decompress([C|T], Acc, Decompressed):-
	\+ integer(C),
	append(Acc, [C], NewAcc),
	decompress(T, NewAcc, Decompressed).


compress(Uncompressed, Compressed):-
	compress(Uncompressed, [], Compressed).

compress([], Acc, RevAcc):-
	reverse(Acc, RevAcc).

compress(Uncompressed, Acc, Compressed) :-
	findall(Id-T, (code(Code, Id), append(Code, T, Uncompressed)), Poss),  % must find all solution all at once
	length(Poss, NbPos),
	NbPos > 0,
	!,   % if no match
	member(Id-T, Poss),
	compress(T, [Id|Acc], Compressed).

compress([C|T], Acc, Compressed):-
	compress(T, [C|Acc], Compressed).

optcompression(Uncompressed, Compressed):-
	findall(L-C, (compress(Uncompressed, C), length(C, L)), Poss),
	sort(Poss, [L-C|SortedPossTail]),
	member(L-Compressed, [L-C|SortedPossTail]).
    
-------------- pipeline------------------
http://cplint.ml.unife.it/p/pipeline_exam.pl

pumpjack(1,1,north).
pumpjack(3,6,east).
pumpjack(6,15,west).
pumpjack(21,9,south).

% the question is rather ridiculous!

% PART I: pumpjacks.
pumpjack_output(pumpjack(A,B,north),X,Y) :-
    X is A+1,
    Y is B+2.
pumpjack_output(pumpjack(A,B,east),X,Y) :-
    X is A+2,
    Y is B-1.
pumpjack_output(pumpjack(A,B,south),X,Y) :-
    X is A-1,
    Y is B-2.
pumpjack_output(pumpjack(A,B,west),X,Y) :-
    X is A-2,
    Y is B+1.
    
outputs(Outputs) :-
        % the order of pumpjack(A,B,D) and pumpjack_output(pumpjack(A,B,D),X,Y) matters
    findall((X,Y),(pumpjack(A,B,D),pumpjack_output(pumpjack(A,B,D),X,Y)),Outputs).

overlaps_pumpjack(pumpjack(X1,Y1,_),X,Y) :-
    X >= X1-1, X =< X1+1,
    Y >= Y1-1, Y =< Y1+1.
    
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PART II: Neighbours & Shortest Paths

neighbour(X,Y,X1,Y1) :-
    ( 
     (X1 is X+1, Y = Y1);
     (X1 is X-1, Y = Y1);
     (Y1 is Y-1, X = X1);
     (Y1 is Y+1, X = X1)
    ),
    \+((pumpjack(A,B,D),overlaps_pumpjack(pumpjack(A,B,D),X,Y))).
   
shortest_path(Target,Source,Distance) :-
    shortest_path(Target,[qi(Source,0)],[],Distance).

shortest_path(_Target,Queue,_Visited,_Distance) :-
    Queue = [],
    !,
    fail.
shortest_path(Target,Queue,_Visited,Distance) :-
    Queue = [qi(Target,DTarget)|_],
    !,
    Distance = DTarget.    
    
shortest_path(Target,Queue,Visited,Distance) :-
    Queue      = [qi((X,Y),DNode)|RestQueue],
    DNeighbour is DNode + 1,
    findall(qi((NX,NY),DNeighbour),
	    (
		neighbour(X,Y,NX,NY),
		not(member(qi((NX,NY),_),Visited))
	    ),
	    Neighbours),
    append(RestQueue,Neighbours,NewQueue),
    append(Neighbours,Visited,NewVisited),
    shortest_path(Target,NewQueue,NewVisited,Distance).

shortest_paths(Nodes,Paths) :-
    findall(edge(D,N1,N2),
	    (member(N1,Nodes),
	     member(N2,Nodes),
	     N1 \== N2,
	     shortest_path(N1,N2,D)),
	    Paths).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PART III: finding the minimal spanning tree

minimal_spanning_tree(Nodes,Edges,MST) :-
    sort(Edges,SEdges),
    create_labels(Nodes,Labels),
    iterate_edges(SEdges,Labels,[],MST).
    
iterate_edges(Edges,Labels,OUT,OUT):-
    forall(member(edge(_,N1,N2),Edges),identical_labels(Labels,N1,N2)).

iterate_edges([edge(D,N1,N2)|Edges],Labels,IN,OUT) :-
    \+identical_labels(Labels,N1,N2),!,
    unify_labels(Labels,N1,N2),
    iterate_edges(Edges,Labels,[edge(D,N1,N2)|IN],OUT)
    ;
    iterate_edges(Edges,Labels,IN,OUT).

plan(MST) :-
    outputs(Outputs),
    shortest_paths(Outputs,Paths),
    minimal_spanning_tree(Outputs,Paths,MST).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DO NOT EDIT BELOW THIS LINE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicates to create, test and unify labels. 

create_labels(Nodes,Labels) :- findall(N-_,member(N,Nodes),Labels).
identical_labels(Labels,N1,N2) :-
    member(N1-L1,Labels),
    member(N2-L2,Labels),
    !,
    L1 == L2.
unify_labels(Labels,N1,N2) :-
    member(N1-L1,Labels),
    member(N2-L2,Labels),
    !,
    L1 = L2.

   
    
