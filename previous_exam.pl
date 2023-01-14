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
    
    
    
