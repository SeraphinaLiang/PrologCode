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


