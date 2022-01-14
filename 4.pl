%------------junior interpreter-----------------

% The mini-meta-interpreter without memoization.
interpret((G1,G2)) :- !,
    interpret(G1),
    interpret(G2).

interpret(true) :- !.

interpret(Head) :-
    clause(Head,Body),
    interpret(Body).

% The mini-meta-interpreter with memoization (including for built-ins).
interpret_mem(Query) :-
	interpret_mem(Query,[],_).

interpret_mem((G1,G2),Mem,MemOut) :- !, 
    interpret_mem(G1,Mem,Mem1), 
    interpret_mem(G2,Mem1,MemOut).

interpret_mem(true,Mem,Mem) :- !.

interpret_mem(Head,Mem,MemOut) :- 
	( member(Head,Mem) ->
		MemOut = Mem
	;
		clause(Head,Body), 
		interpret_mem(Body,Mem,Mem1),
		MemOut = [Head|Mem1]
	).

% fib/2
fib(0,1).
fib(1,1).
fib(N,F) :-
    N > 1,
    N2 is N - 2,
    fib(N2,F2),
    N1 is N - 1,
    fib(N1,F1),
    F is F1 + F2.

%-------------holiday lights------------   https://swish.swi-prolog.org/p/RBGRTH.pl

% Example knowledge base
highway(1,2,yellow).
highway(2,3,blue).
highway(1,3,yellow).
highway(1,2,c).
highway(2,3,a).
highway(1,3,b).
highway(3,5,a).
highway(3,4,c).
highway(5,4,d).

color(X,Color):-
    highway(X,_,Color);
    highway(_,X,Color).

node(X):-
    highway(_,X,_);
    highway(X,_,_).

check():-
    findall(highway(A,B,C),highway(A,B,C),List), % List : all highways
    sort(List,L),!, % Duplicates are removed
    check_all_node(L).
    
check_all_node(List):-
    findall(X,node(X),ListNode),
    sort(ListNode,SortListNode), % [1,2]
    checkNode(SortListNode,List).

checkNode([],_).  % manually check all node
checkNode([X|List],L):-
    check_even_at(X,L),
    check_colors_at(X,L),
    checkNode(List,L).

check_even_at(X,L):-
    list_k(X,L,List), % highway list with node k
    sort(List,SL),
    length(SL,Length),!, % backtracking affect result
    N is Length mod 2,
    N =:= 0.

check_colors_at(X,L):-
    list_k(X,L,List), % highway list with node k
    sort(List,SL),
    check_color(X,SL).

check_color(X,L):-
    color(X,Color),
    check_yes(X,Color,L,0,Len1),
    check_no(X,Color,L,0,Len2),!, % backtracking affect result
    Len2 >= Len1.

list_k(_,[],[]).
list_k(X,[highway(X,A,B)|L1],[highway(X,A,B)|L2]):-
    list_k(X,L1,L2).
list_k(X,[highway(A,X,B)|L1],[highway(A,X,B)|L2]):-
    list_k(X,L1,L2).
list_k(X,[_|L1],L2):-
    list_k(X,L1,L2).

check_yes(_,_,[],Len,Len).
check_yes(X,Color,[highway(X,_,Color)|L],Len,Length):-
    Len1 is Len + 1,
    check_yes(X,Color,L,Len1,Length).
check_yes(X,Color,[highway(_,X,Color)|L],Len,Length):-
    Len1 is Len + 1,
    check_yes(X,Color,L,Len1,Length).
check_yes(X,Color,[_|L],Len,Length):-
    check_yes(X,Color,L,Len,Length).

check_no(_,_,[],Len,Len).
check_no(X,Color,[highway(X,_,Color)|L],Len,Length):-
    check_no(X,Color,L,Len,Length).
check_no(X,Color,[highway(_,X,Color)|L],Len,Length):-
    check_no(X,Color,L,Len,Length).
check_no(X,Color,[_|L],Len,Length):-
    Len1 is Len + 1,
    check_no(X,Color,L,Len1,Length).




