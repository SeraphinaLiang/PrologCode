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

%-------------holiday lights------------

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

check():-
    highway(X,_,_),
    check_even_at(X),
    check_colors_at(X),
    highway(_,Y,_),
    check_even_at(Y),
    check_colors_at(Y).

check_even_at(X):-
    setof(X,highway(X,_,_),L1),
    length(L1,Length1),
    setof(X,highway(_,X,_),L2),
    length(L2,Length2),
    (Length1+Length2) mod 2 =:= 0.

check_colors_at(X):-
    setof(X,highway(X,_,_),L1),
    setof(X,highway(_,X,_),L2),
    append(L1,L2,L),
    check_color(X,L).

check_color(X,L):-
    color(X,Color),
    check_yes(X,Color,L,0,Len1),
    check_no(X,Color,L,0,Len2),
    Len2 >= Len1.

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



