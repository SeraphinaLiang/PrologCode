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

%p2 win & p1 lose
play_game(player([],_),player([_|_],_),_,_,_,_).
%p1 win & p2 lose
play_game(player([_|_],_),player([],_),_,_,_,_).
% both players out of blocks - draw
play_game(player([],_),player([],_),_,_,_,_).
% add draw to other
play_game(player(_,_),player(_,_),_,_,_,_).


% play_game(player1,player2,Table,Bag,newplayer1,newplayer2)
play_game(P1A,P2A,Table,Bag,P1B,P2B):-
    play(P1A,P1B,Table,Bag,Newtable1,Newbag1),
    play(P2A,P2B,Newtable1,Newbag1,Newtable2,Newbag2),
    % not end
    \+is_end(P1B,P2B,Newtable2,Newbag2),
    play_game(P1B,P2B,Newtable2,Newbag2,_,_);
    % end
    is_end(P1B,P2B,_,_).

is_end(player(_,A1),player(_,A2),_,_):-
    %p2 win & p1 lose
    play_game(player([],A1),player([_|_],A2),_,_,_,_),
    addEnding(A1,lose),addEnding(A2,win);
    %p1 win & p2 lose
    play_game(player([_|_],A1),player([],A2),_,_,_,_),
    addEnding(A1,win),addEnding(A2,lose);
    % both players out of blocks - draw
    play_game(player([],A1),player([],A2),_,_,_,_),
    addEnding(A1,draw),addEnding(A2,draw);
    % add draw to other player
    play_game(player(_,A1),player(_,A2),_,_,_,_),
    member(draw,A1),\+member(draw,A2),addEnding(A2,draw);
    play_game(player(_,A1),player(_,A2),_,_,_,_),
    member(draw,A2),\+member(draw,A1),addEnding(A1,draw).

% play(playerOrigin,playerAfter,Table,Bag,NewTable,NewBag)
play(P1,P2,T,B,NT,NB):-
    do_playrow(P1,P2,T,B,NT,NB),
    do_playblock(P1,P2,T,B,NT,NB),
    do_draw(P1,P2,T,B,NT,NB).

do_playrow(P1,P2,T,B,NT,NB):-
    play_colorrow(P1,P2,T,B,NT,NB);
    play_numberrow(P1,P2,T,B,NT,NB).

%play_colorrow(player_origin(Block,Actions),player_after(newBlocks,newAction),Table,Bag,NewTable,NewBag)
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

% extend crow or nrow
do_playblock(player(Blocks,Actions),player(NB,NA),T,_,[TheRow|NT],_):-
    extendcrow(Blocks,T,TheBlock,TheRow,NB,NT),
    addEnding(Actions,playblock(TheBlock,crow(TheRow))),
    list_to_set(Actions,NA);
    extendnrow(Blocks,T,TheBlock,TheRow,NB,NT),
    addEnding(Actions,playblock(TheBlock,nrow(TheRow))),
    list_to_set(Actions,NA).

extendcrow([],_,_,_,_,_).
extendcrow(_,[],_,_,_,_).
extendcrow([B|Blocks],[crow(R)|T],B,TheR,Blocks,T):-
    addEnding(B,R),
    sort(R,TheR),
    is_crow(crow(TheR));
    extendcrow([B|Blocks],T,B,TheR,Blocks,T);
    extendcrow(Blocks,[crow(R)|T],B,TheR,Blocks,T).

extendnrow([],_,_,_,_,_).
extendnrow(_,[],_,_,_,_).
extendnrow([B|Blocks],[nrow(R)|T],B,TheR,Blocks,T):-
    addEnding(B,R),
    is_nrow(nrow(R)),
    TheR = R;
    TheR = [B|R],
    is_nrow(nrow(TheR));
    extendnrow([B|Blocks],T,B,TheR,Blocks,T);
    extendnrow(Blocks,[nrow(R)|T],B,TheR,Blocks,T).

% play(player(Blocks,Actions),new_player(Blocks,Actions),Table,Bag,NewTable,NewBag)
do_draw(player(Blocks,Actions),player([Block|Blocks],NA),_,[Block|Others],_,Others):-
    addEnding(Actions,draw(Block)),
    list_to_set(Actions,NA).

% a player has to draw new block & bag is empty - draw
do_draw(player(_,Actions),player(_,NA),_,[],_,_):-
    addEnding(Actions,draw),
    list_to_set(Actions,NA).

pick3blocks(Blocks,[A,B,C],Other):-
    permutation(Blocks,[A,B,C|Other]).

permutation([],[]).
permutation(A,[Head|B]):-
    delete(A,Head,A1),
    permutation(A1,B).

addEnding([],_).
addEnding([Ending],Ending):-
    addEnding([],_).
addEnding([_|Actions],Ending):-
    addEnding(Actions,Ending).
    
    
% test case :
P1Blocks = [block(1,red),block(2,red),block(3,red),block(2,blue)],
P2Blocks = [block(5,red),block(5,yellow),block(5,black),block(4,red)],
play_game(player(P1Blocks,P1Act),player(P2Blocks,P2Act),[],[block(5,blue)]).


% ----------my version 2 with lots of build in-------------------------  https://swish.swi-prolog.org/p/bYDKyewp.pl
valid_table(Table):-
    check_row(Table).

check_row([]).
check_row([Row|L]):-
    check_3_length(Row),
    color_rule(Row),
    number_rule(Row),
    check_row(L).

check_3_length(crow(List)):-
    length(List,Len),
    Len >= 3.
check_3_length(nrow(List)):-
    length(List,Len),
    Len >= 3.

%color rule for crow
color_rule(crow(List)):-
    identical_number(List),
    unique_color(List).
color_rule(nrow(_)).

% number rule for nrow
number_rule(nrow(List)):-
    identical_color(List),
    succ_number(List).
number_rule(crow(_)).

succ_number(L):-
    findall(N,member(block(N,_),L),List),!,
    consecutive(List).

consecutive([_]).
consecutive([X,Y|L]):-
    X + 1 =:= Y,
    consecutive([Y|L]).

identical_number(L):-
    findall(N,member(block(N,_),L),List),
    sort(List,SL),
    length(SL,SLN),
    SLN =:= 1.

identical_color(L):-
    findall(C,member(block(_,C),L),List),
    sort(List,SL),
    length(SL,SLN),
    SLN =:= 1.

unique_color(L):-
    findall(C,member(block(_,C),L),List),
    sort(List,SL),
    length(SL,SLN),
    length(List,List_len),
    SLN =:= List_len.
    
----question 2 -- unfinish--------

% draw
play_game(player([],[draw|A]),player([],[draw|B]),_,_):-
    play_game(player([],A),player([],B),_,_).
play_game(player([],_),player([],_),_,_).

% one player win
play_game(player([],[win|A]),player(_,[lose|B]),_,_):- 
    play_game(player([],A),player(_,B),_,_).
play_game(player([],_),player(_,_),_,_).

play_game(player(_,[lose|A]),player([],[win|B]),_,_):- 
    play_game(player(_,A),player([],B),_,_).
play_game(player(_,_),player([],_),_,_).

play_game(P1,P2,Table,Bag):- 
    (   % P1 playcrow
    playrow_action(crow(Blocks),Newrow),
    update_player_extract_block(P1,NP1,Blocks,playrow(Blocks)),
    NT1 = [Newrow|Table];
        % P1 playnrow
    playrow_action(nrow(Blocks),Newrow),
    update_player_extract_block(P1,NP1,Blocks,playrow(Blocks)),
    NT1 = [Newrow|Table];
        % p1 playblock
    member(Row,Table),
    get_a_block(P1,Block,NPT1),
    playblock_action(Block,Row,Newrow),
    add_action(NPT1,NP1,playblock(Block,Row)),
    delete(Table,Row,Tabledelete),
    NT1 = [Newrow|Tabledelete];
        % p1 draw block
    draw_block_action(Bag,Block,Newbag)
    )
    ,
    (    % P2 playrow
    playrow_action(crow(Blocks),Newrow),
    update_player_extract_block(P2,NP2,Blocks,playrow(Blocks)),
    NT2 = [Newrow|NT1];
         % P2 playnrow
    playrow_action(nrow(Blocks),Newrow),
    update_player_extract_block(P2,NP2,Blocks,playrow(Blocks)),
    NT2 = [Newrow|NT1];
         % p2 playblock
    member(Row,NT1),
    get_a_block(P2,Block,NPT2),
    playblock_action(Block,Row,Newrow),
    add_action(NPT2,NP2,playblock(Block,Row)),
    delete(NT1,Row,Tabledelete),
    NT2 = [Newrow|Tabledelete];
         % p2 draw block
    draw_block_action(Bag,Block,Newbag)
    ),
    play_game(NP1,NP2,NT2,Newbag).

add_action(player(B,A),player(B,[Action|A]),Action).

get_a_block(player(B,A),Block,player(NB,A)):-
    member(Block,B),
    delete(B,Block,NB).

update_player_extract_block(player(OB,OA),player(NB,NA),Blocks,Action):-
    select(Blocks,OB,NB),
    NA = [Action|OA].

playrow_action(crow(Blocks),Newrow):-
    length(Blocks,Length),
    Length =:= 3,
    identical_number(Blocks),
    unique_color(Blocks),
    sort(Blocks,Newrow).

playrow_action(nrow(Blocks),Newrow):-
    length(Blocks,Length),
    Length =:= 3,
    identical_color(Blocks),
    succ_number(Blocks),
    sort(Blocks,Newrow).

playblock_action(Block,Row,Newrow):-
    %on the start or end of an existing nrow
    playblock_action_nrow(Block,Row,Newrow);
    %in an existing crow and resorting the row
    playblock_action_crow(Block,Row,Newrow).
    
playblock_action_nrow(B,nrow(L),nrow(NL)):-
    append([B],L,NL);
    append(L,[B],NL).

playblock_action_crow(B,crow(L),crow(NL)):-
    append([B],L,Temp),
    sort(Temp,NL).

draw_block_action([Block|Newbag],Block,Newbag).
    

    
