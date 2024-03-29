--------------------- compression-------------------------
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

-------------solutions--compression---------


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
    
iterate_edges([],_,OUT,OUT):-.

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

-----------------Sokoban------------------

http://cplint.ml.unife.it/p/Sokoban.pl

reachable(Sokaban,(X,Y)):-
    get_unoccupy(Sokaban,Free),
    get_player_pos(Sokaban,(O1,O2)),
    findroute((O1,O2),(X,Y),Free).

findroute((TX,TY),(TX,TY),_).
findroute((PX,PY),(TX,TY),Free):-
    FX is PX,!,
    member((FX,FY),Free),
    abs(PY- FY,1),!,
    delete(Free,(FX,FY),New),
    findroute((FX,FY),(TX,TY),New).
findroute((PX,PY),(TX,TY),Free):-
    FY is PY,!,
    member((FX,FY),Free),
    abs(PX - FX,1),!,
    delete(Free,(FX,FY),New),
    findroute((FX,FY),(TX,TY),New).
    
get_player_pos(sokoban(_,_,(O1,O2),_),(O1,O2)).

get_unoccupy(sokoban(W,H,_,L),Unoccupy):-
    W1 is W-1,
    H1 is H-1,
    numlist(0,W1,N1), % 0...W-1
    numlist(0,H1,N2), % 0...H-1
    findall((X,Y),(member(X,N1),member(Y,N2)),Board),
    occupy_wall(L,Occupy1),
    findall((C1,C2),member(crate(_,C1,C2),L),Occupy2),
    append(Occupy1,Occupy2,Occupy),
    subtract(Board,Occupy,Unoccupy).

occupy_wall([],[]).
occupy_wall([wall((X1,Y1),(X2,Y2))|Rest],[OCU|Occupy]):-
    %vertical
    X1 =:= X2,
    Y1 =< Y2,
    Diff is Y2 - Y1,!,
    numlist(0,Diff,Add),
    findall((X1,NY),(member(AD,Add),NY is AD+Y1),OCU),
    occupy_wall(Rest,Occupy)
    ;
    % horizontal
    Y1 =:= Y2,
    X1 =< X2,
    Diff is X2-X1,!,
    numlist(0,Diff,Add),
    findall((XN,Y1),(member(AD,Add),XN is AD+X1),OCU),
    occupy_wall(Rest,Occupy).

occupy_wall([_|Rest],Occupy):-
    occupy_wall(Rest,Occupy).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(lists)).

writeall([]) :- writeln('   fail').
writeall(List) :- List \== [], writeall(List,'   ','').
writeall([],_,_).
writeall([X|R],Prefix,Postfix) :-
    write(Prefix),
    write(X),
    writeln(Postfix),
    writeall(R,Prefix,Postfix).

test_assignment_1 :-
    Sok1 = sokoban(7,7, (1,1), [wall((0,0),(6,0)), wall((6,0),(6,6)), wall((0,6),(6,6)), wall((0,0),(0,6)),
                                 wall((3,0),(3,2)), wall((3,4),(3,6)), crate(1,3,3), storage(1,3)]),
    findall(P,reachable(Sok1,P),L),
    writeln('\nALL SOLUTIONS TO ASSIGNMENT 1.1:'),
    writeall(L).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sokoban(Width, Height, CurrPos, Field)
reachable(sokoban(Width, Height, CurrPos, Field), (X,Y)):-
    getoccupiedpos(Field,Occupy),
    getfreepos(Occupy,Width,Height,Freepos),
    findreachable(Freepos,CurrPos,Reachpos),
    sort(Reachpos,RP),
    member((X,Y),RP).


findreachable([],C,[C]).
findreachable(Freepos,Cur,[P1|Reachpos]):-
    select(P1,Freepos,NF),
    adjancent(P1,Cur),
    findreachable(NF,P1,Reachpos).

adjancent((X,Y1),(X,Y2)):-!,
    abs(Y1-Y2) =:= 1.
adjancent((X1,Y),(X2,Y)):-!,
    abs(X1-X2) =:= 1.

getfreepos(Occupy,Width,Height,Freepos):-
    % must do like this
    numlist(0,Width,WL),
    numlist(0,Height,HL),
    findall((X,Y),(member(X,WL),member(Y,HL)),Board),
    freePos(Board,Occupy,Freepos).


% subset
freePos(L,[],L).
freePos([X|L1],[X|L2],P):-
    freePos(L1,L2,P).
freePos(L1,[_|L2],P):-
    freePos(L1,L2,P).


%---------done------------------------------

getoccupiedpos(Field,Occupy):-
    getWall(Field,Walls),
    findall((X1,Y1),member(crate(_,X1,Y1),Field),Listofcrate),
    append(Listofcrate,Walls,Occupy).

getWall(Fields,Walls):-
    findall(wall(A,B),member(wall(A,B),Fields),L1),
    getExtendPos(L1,Pos),
    sort(Pos,Walls).

getExtendPos([],[]).
getExtendPos([W|LW],Pos):-
    getbrick(W,B),
    append(B,Pos1,Pos),
    getExtendPos(LW,Pos1).

getbrick(wall((X,Y),(X,Y)),[(X,Y)]).
getbrick(wall((X1,Y),(X2,Y)),[(X1,Y)|Bricks]):-
    X11 is X1+1,
    getbrick(wall((X11,Y),(X2,Y)),Bricks).
getbrick(wall((X,Y1),(X,Y2)),[(X,Y1)|Bricks]):-
    Y11 is Y1+1,
    getbrick(wall((X,Y11),(X,Y2)),Bricks).
    
