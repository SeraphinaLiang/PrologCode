% minmaxtree(node(1-node(2-node(1-leaf(who), 3-leaf(knew)),
% 4-node(0-leaf(prolog), 1-leaf(was))), 3-node(4-node(5-leaf(such),
% 2-leaf(fun)), 6-node(2-leaf(!), 4-leaf(!)))), Result).
% Result = prolog

minmaxtree(T,R):- minmaxtree_min(T,R).

% node(C1-node(T1),C2-node(T2)) ----> WRONG, cant unify, !! cannot mix terms and variables !!

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

-------------------------------------------------------------------
teaches(holvoet,bvp).
teaches(moens,bvp).
teaches(demoen,fi).
teaches(dedecker,socs).
teaches(holvoet,swop).
teaches(jacobs,swop).
teaches(demoen,ai).
teaches(deschreye,ai).

takes(joachim,bvp).
takes(joachim,fi).
takes(joachim,ai).
takes(matthias,bvp).
takes(matthias,ai).
takes(thomas,socs).
takes(thomas,swop).
takes(ingmar,swop).

takes_same_course(X,Y):-
    findall(pair(X1,Y1),(takes(X1,A),takes(Y1,A)),L),
    sort(L,LS),
    member(pair(X,Y),LS),
    X \== Y.

teach_same_course(X,Y):-
    findall(pair(X1,Y1),(teaches(X1,A),teaches(Y1,A)),L),
    list_to_set(L,LS),
    member(pair(X,Y),LS),
    X \== Y.

teaches_multiple_courses(Y):-
    findall(F,teaches(F,_),L),
    sort(L,SY),
    include(more_than_one,SY,SSY),
    member(Y,SSY).

more_than_one(I):-
    findall(teaches(I,A),teaches(I,A),L),
    length(L,LEN),
    LEN > 1.


-----------------------------------------------
% A simple solution
map(_, [], []).
map(P, [X|Xs], [Y|Ys]) :-
    Call =.. [P,X,Y],
    call(Call),
    map(P,Xs,Ys).

increment(X,Y) :- Y is X + 1.

decrement(X,Y) :- Y is X - 1.

% A more comprehensive solution that allows for calls such as:
% map1(plus(2),[2,3,4],[4,5,6]).

map1(_,[],[]).
map1(P, [X|Xs], [Y|Ys]) :-
    P =.. Args,
    append(Args, [X,Y], NewArgs),
    Call =.. NewArgs,
    call(Call),
    map1(P,Xs,Ys).

plus(X,Y,Z):-
    Z is X + Y.
--------------------------------
%% translate/2
%%
%% ?- translate([def(a), use(a), use(b), use(c), def(c), def(b)], L).
%% L = [asgn(a, 1), use(1), use(3), use(2), asgn(c, 2), asgn(b, 3)]
%%
%% The input list is a sequence of def/1 and use/1 terms.
%% Note that e.g. use(b) occurs in the list before before def(b),
%% The symbols a,b,c,... are mapped to an integer number (e.g. representing
%% their absolute address in the symbol table).
%% The order of the def/1 terms in the input list determines the number for the
%% symbols, (e.g. their address in the symboltable).
%% In the translated list we replace def(sa) by asgn(sa,nb) and use(sa) by
%% use(nb) with nb the number associated with sa.
%%
%% For the input list [def(a), use(a), use(b), use(c), def(c), def(b)] ,
%% we will use number 1 for a, number 2 for c and number 3 for b.
%%
%% This illustrates the power of unification and free variables:
%% translation can be done with one pass (instead of the classical 2 passes).


translate(InL, OutL) :-
    map(InL, OutL, _, 1).


%% map/4
%% 1st arg. : input list
%% 2nd arg. : output list
%% 3rd arg. : open ended list: symboltable,
%%     a listelement associates a symbol with an absolute address.
%%     The absolute address will be instantiated when a def occurs in the
%%     input list.
%% 4th arg. : counter indicating the next available address


map([],[],T,N) :-
    L is N - 1,
    length(T,L).
    
map([def(A) | InL], [asgn(A,N) | OutL], Table, N) :-
    member(asgn(A,N), Table),
    !,
    N1 is N + 1,
    map(InL,OutL,Table,N1).
map([use(A)|InL], [use(NB)|OutL], Table, N) :-
    member(asgn(A,NB), Table),
    !,
    map(InL,OutL,Table,N).
