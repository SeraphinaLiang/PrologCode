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
    
    
