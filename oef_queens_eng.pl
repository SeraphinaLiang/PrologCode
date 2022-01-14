
% A naive solution.

% queens/2
queens(N,Ys) :-
  generate(1,N,Xs),
  permutation(Xs,Ys),
  safe(Ys).

% generate/3
generate(N,N,[N]).
generate(M,N,[M|List]) :-
  M < N, M1 is M + 1,
  generate(M1,N,List).

% permutation/2
permutation([],[]).
permutation(List,[Elem|Perm]) :-
  select(Elem,List,Rest),
  permutation(Rest,Perm).

% safe/1
safe([]).
safe([First|Queens]) :-
  noattack(First,Queens,1),
  safe(Queens).

% noattack/2
noattack(_,[],_).
noattack(Queen,[First|Queens],N) :-
  % permutation ensures that no two queens are on same row
  abs(Queen - First) =\= N, % ensure no two queens are on same diagonal
  N1 is N + 1,
  noattack(Queen,Queens,N1).


% A solution with freeze. This version chooses a row position for a queen in a 
% certain column, and immediately checks whether this queen can attack a queen
% to her left.

% frqueens/2
frqueens(N,Ys) :-
  generate(1,N,Xs,Ys),
  frsafe(Ys,[]), % freeze first one (no queens on left yet) 
  permutation(Xs,Ys).

% generate/4
generate(N,N,[N],[_]).
generate(M,N,[M|List],[_|Vars]) :-
  M < N, M1 is M + 1,
  generate(M1,N,List,Vars).

% frsafe/2
frsafe([],_).
frsafe([Queen|RightQueens],LeftQueens) :-
  freeze(Queen,frs(RightQueens,Queen,LeftQueens)).

% frs/3
frs(RightQueens,Queen,LeftQueens):-
  frnoattacktoleft(Queen,LeftQueens,1),
  frsafe(RightQueens,[Queen|LeftQueens]). % freeze next one

% frnoattacktoleft/3
frnoattacktoleft(_,[],_).
frnoattacktoleft(Queen,[First|LeftQueens],N):-
  % Queen =\= First, % ensured by using permutations!
  abs(Queen-First) =\= N,
  N1 is N + 1,
  frnoattacktoleft(Queen,LeftQueens,N1).


% It is possible to write a solution that does not use freeze, but tests as soon
% as possible. This performs even better.

% coqueens/2
coqueens(N,Ys):-
	generate(1,N,Xs,_),
	refine(Xs,[],Ys).

% refine/3
refine([],_,[]).
refine(Kands,Links,[Y|Ys]) :-
	select(Y,Kands,Kands1),
	noattack(Y,Links,1),
	refine(Kands1,[Y|Links],Ys).

