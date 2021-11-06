%-----------question 1----------------
valid_table([]).
valid_table([Row|Others]):- 
    valid_row(Row),  % 第一行符不符合
    valid_table(Others).  % 看其他行是否合格

is_crow(crow(L)):-    % 是crow，且每个row至少有三个block
    length(L,N),
    N>2.
is_nrow(nrow(L)):-    % 是nrow，且每个row至少有三个block
    length(L,N),
    N>2.

valid_row([]).
valid_row(Row):-
    is_crow(Row),   % 够长的crow
    valid_crow(Row);
    is_nrow(Row),   % 够长的nrow
    valid_nrow(Row).

valid_crow(crow([])).
valid_crow(crow(List)):-    % 类型转换，list要递归
    color_noconflict(List).

color_noconflict([]).
color_noconflict([Head|Tail]):-   % 这一行是否冲突
    color_rule(Head,Tail),   % 第1个和这行剩下的不冲突
    color_noconflict(Tail).  % 剩下的互相都不冲突

valid_nrow(nrow([])).
valid_nrow(nrow(List)):-
    number_noconflict(List).

number_noconflict([]).
number_noconflict([Head|Tail]):-
    number_rule(Head,Tail,1),
    number_noconflict(Tail).

color_rule(_,[]).
color_rule(A,[B|L]):-
    identical_number(A,B),   % A and B not conflict
    unique_color(A,B),
    color_rule(A,L).   % A is not conflict with the rest of elements in the list

number_rule(_,[],_).
number_rule(A,[B|L],N):-
    identical_color(A,B),
    sorted(A,B,N),  % number形成有序序列
    M is N+1,  % 算a和b的差值，每次增加1
    number_rule(A,L,M).


identical_number(block(X,_),block(X,_)).
identical_color(block(_,C),block(_,C)).

unique_color(block(_,A),block(_,B)):-
    A \== B.

sorted(block(X,_),block(Y,_),N):-
    X is Y - N.
    
%----------question 2-----------

addEnding([],_).
addEnding([Ending],Ending):-
    addEnding([],_).
addEnding([Head|Actions],Ending):-
    addEnding(Actions,Ending).

%p2 win & p1 lose
play_game(player([],Action1),player([_|_],Action2),_,_):-
    addEnding(Action1,lose),addEnding(Action2,win).
%p1 win & p2 lose
play_game(player([_|_],Action1),player([],Action2),_,_):-
    addEnding(Action1,win),addEnding(Action2,lose).
% both players out of blocks - draw
play_game(player([],Action1),player([],Action2),_,_):-
    addEnding(Action1,draw),addEnding(Action2,draw).
% a player has to draw new block & bag is empty - draw
do_draw(player(_,Actions),_,[],_,_):-
    addEnding(Actions,draw).
play_game(player(_,A1),player(_,A2),_,_):-
    member(draw,A1),\+member(draw,A2),addEnding(A2,draw);
    member(draw,A2),\+member(draw,A1),addEnding(A1,draw).

play_game(P1,P2,Table,Bag):-
    play(P1,Table,Bag,Newtable1,Newbag1),
    play(P2,Newtable1,Newbag1,Newtable2,Newbag2),
    play_game(P1,P2,Newtable2,Newbag2).

% play(player(Blocks,Actions),Table,Bag,NewTable,NewBag)
play(Player,T,B,NT,NB):-
    do_playrow(Player,T,B,NT,NB),
    do_playblock(Player,T,B,NT,NB),
    do_draw(Player,T,B,NT,NB).

do_playrow(Player,T,B,NT,NB):-
    play_colorrow(Player,T,B,NT,NB);
    play_numberrow(Player,T,B,NT,NB).

play_colorrow(player(Blocks,Actions),player(NB,NA),T,_,[crow(Picksort)|T],_):-
    pick3blocks(Blocks,Picked,NB),
    is_crow(crow(Picked)),
    sort(Picked,Picksort),
    addEnding(Actions,playrow(crow(Picksort))),
    list_to_set(Actions,NA).

play_numberrow(player(Blocks,Actions),player(NB,NA),T,_,[nrow(Picksort)|T],_):-
    pick3blocks(Blocks,Picked,NB),
    is_nrow(nrow(Picked)),
    sort(Picked,Picksort),
    addEnding(Actions,playrow(nrow(Picksort))),
    list_to_set(Actions,NA).

do_playblock(player(Blocks,Actions),T,B,NT,NB):-!.

do_draw(player(Blocks,Actions),T,B,NT,NB):-!.



pick3blocks(Blocks,[A,B,C],Other):-
    permutation(Blocks,[A,B,C|Other]).

permutation([],[]).
permutation(A,[Head|B]):-
    delete(A,Head,A1),
    permutation(A1,B).



