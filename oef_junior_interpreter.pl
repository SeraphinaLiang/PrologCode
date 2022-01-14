
% The mini-meta-interpreter without memoization.
interpret((G1,G2)) :- !,
    interpret(G1),
    interpret(G2).

interpret(is(X,Y)) :- !,
    X is Y.

interpret('>'(X,Y)) :- !,
    X > Y.

interpret(true) :- !.

interpret(Head) :-
    clause(Head,Body),
    interpret(Body).

% The mini-meta-interpreter without memoization for built-ins.
interpret_mem(Query) :-
	interpret_mem(Query,[],_).

interpret_mem(is(X,Y),Mem,Mem) :- !, 
	X is Y.

interpret_mem('>'(X,Y),Mem,Mem) :- !, 
	X > Y.

% Uncomment the following clause to provide a general implementation for all built-ins.
% interpret_mem(G,Mem,Mem) :-
%    predicate_property(G,built_in),
%    !,
%    call(G).

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

% The mini-meta-interpreter with memoization for built-ins.
interpret_mem2(Query) :-
	interpret_mem2(Query,[],_).

interpret_mem2(is(X,Y),Mem,MemOut) :- !, 
	( member(is(X,Y),Mem) ->
		MemOut = Mem
	;
		X is Y,
		MemOut = [is(X,Y)|Mem]
	).

interpret_mem2('>'(X,Y),Mem,MemOut) :- !, 
	( member('>'(X,Y),Mem) ->
		MemOut = Mem
	;
		X > Y,
		MemOut = ['>'(X,Y)|Mem]
	).

% Uncomment the following clause to provide a general implementation for all built-ins.
% interpret_mem2(G,Mem,MemOut) :-
%    predicate_property(G,built_in),
%    !,
%    (member(G,Mem) ->
%      MemOut = Mem
%    ;
%      call(G),
%      MemOut = [G|Mem]
%    ).

interpret_mem2((G1,G2),Mem,MemOut) :- !, 
    interpret_mem2(G1,Mem,Mem1), 
    interpret_mem2(G2,Mem1,MemOut).

interpret_mem2(true,Mem,Mem) :- !.

interpret_mem2(Head,Mem,MemOut) :- 
	( member(Head,Mem) ->
		MemOut = Mem
	;
		clause(Head,Body), 
		interpret_mem2(Body,Mem,Mem1),
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
