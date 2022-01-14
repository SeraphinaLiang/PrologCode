%%%%%%%%%%%%%%%%%%
%% ASSIGNMENT 1 %%
%%%%%%%%%%%%%%%%%%

% Small helper predicates
%%%%%%%%%%%%%%%%%%%%%%%%%

% Succeeds if all members are distinct
distinct([]).
distinct([X|Xs]) :- \+ member(X,Xs), distinct(Xs).

% Succeeds if all members are identical
identical([]).
identical([X|Xs]) :- \+ (member(Y,Xs), Y \= X).

% Succeeds if the given list consists of subsequent numbers (no duplicates)
subsequent([]).
subsequent(List) :-
    min_list(List,Min),
    max_list(List,Max),
    numlist(Min,Max,List).

% Succeeds if the given row meets all requirements
valid_row(crow(Blocks)) :-
    % At least 3 blocks
    length(Blocks,NbBlocks), NbBlocks >= 3,
    % All colors must be distinct
    findall(Color,member(block(_Number,Color),Blocks),Colors),
    distinct(Colors),
    % All numbers must be identical
    findall(Number,member(block(Number,_Color),Blocks),Numbers),
    identical(Numbers).
valid_row(nrow(Blocks)) :-
    % At least 3 blocks
    length(Blocks,NbBlocks), NbBlocks >= 3,
    % All colors must be identical
    findall(Color,member(block(_Number,Color),Blocks),Colors),
    identical(Colors),
    % The numbers must form a sequenced series
    findall(Number,member(block(Number,_Color),Blocks),Numbers),
    subsequent(Numbers).

% Succeeds if each row of the table meets the requirements
valid_table(Table) :- 
    \+ (member(Row,Table) , \+ valid_row(Row)).

%%%%%%%%%%%%%%%%%%
%% ASSIGNMENT 2 %%
%%%%%%%%%%%%%%%%%%

% Extend a row on the table with the given block
add_block(Block,Table,[crow(SortedRow)|TablePopped]) :-
    select(crow(Row),Table,TablePopped),
    NewRow = [Block|Row],
    sort(NewRow,SortedRow),
    valid_row(crow(SortedRow)).
add_block(Block,Table,[nrow(NewRow)|TablePopped]) :-
    select(nrow(Row),Table,TablePopped),
    (   append([Block],Row,NewRow) ; append(Row,[Block],NewRow) ),
    valid_row(nrow(NewRow)).

% Create a new row with the given blocks
create_row(Blocks,RemBlocks,crow(Row)) :-
    % Remove duplicates
    findall((RowS,RemBlocks),(
      % Find 3 matching numbered blocks
      select(block(Number,C1),Blocks,Blocks1),
      select(block(Number,C2),Blocks1,Blocks2),
      select(block(Number,C3),Blocks2,RemBlocks),
      Row = [block(Number,C1),block(Number,C2),block(Number,C3)],
      % Check whether the row is valid
      valid_row(crow(Row)),
      sort(Row,RowS)),
    AllRows),
    list_to_set(AllRows,RowsDup),
    member((Row,RemBlocks),RowsDup).
create_row(Blocks,RemBlocks,nrow(Row)) :-
    % Remove duplicates
    findall((Row,RemBlocks),(
      % Find 3 matching colored blocks
      select(block(N1,Color),Blocks,Blocks1), 
      N2 is N1 + 1, N3 is N1 + 2,
      select(block(N2,Color),Blocks1,Blocks2),
      select(block(N3,Color),Blocks2,RemBlocks),
      Row = [block(N1,Color),block(N2,Color),block(N3,Color)],
      % Check whether the row is valid
      valid_row(nrow(Row))),
    AllRows),
    list_to_set(AllRows,RowsDup),
    member((Row,RemBlocks),RowsDup).

% Extend an existing row
put_block(Blocks,Table,playblock(Block,NewRow),RemBlocks,[NewRow|NewTable]) :-
    select(Block,Blocks,RemBlocks),
    add_block(Block,Table,[NewRow|NewTable]).

% Create a new row
put_block(Blocks,Table,playrow(Row),RemBlocks,[Row|Table]) :-
    create_row(Blocks,RemBlocks,Row).

play_game(player([],[win]),player([_|_P2Blocks],[lose]),_Table,_Bag).
play_game(player([_|_P1Blocks],[lose]),player([],[win]),_Table,_Bag).
play_game(player([],[draw]),player([],[draw]),_Table,_Bag).
play_game(player(P1Blocks,[Action|P1Actions]),player([P2B|P2Blocks],P2Actions),Table,Bag) :-
    put_block(P1Blocks,Table,Action,P1BlocksRem,NewTable),
    play_game(player([P2B|P2Blocks],P2Actions),player(P1BlocksRem,P1Actions),NewTable,Bag).
play_game(player([P1B|P1Blocks],[draw]),player([_P2B|_P2Blocks],[draw]),Table,[]) :-
    \+ put_block([P1B|P1Blocks],Table,_Action,_P1BlocksRem,_NewTable).
play_game(player([P1B|P1Blocks],[draw(Block)|P1Actions]),player([P2B|P2Blocks],P2Actions),Table,[Block|Bag]) :-
    \+ put_block([P1B|P1Blocks],Table,_Action,_P1BlocksRem,_NewTable),
    play_game(player([P2B|P2Blocks],P2Actions),player([Block,P1B|P1Blocks],P1Actions),Table,Bag).
