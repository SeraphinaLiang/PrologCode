loves(romeo,juliet).

loves(juliet,romeo):-loves(romeo,juliet).

male(albert).
male(bob).
male(bill).
male(carl).
male(charlie).
male(dan).
male(edward).

female(alice).
female(betsy).
female(diana).

parent(albert,bob).
parent(albert,betsy).
parent(albert,bill).

parent(alice,bob).
parent(alice,betsy).
parent(alice,bill).

parent(bob,carl).
parent(bob,charlie).

related(X,Y) :-
  parent(X,Y).
  
related(X,Y) :-
  parent(X,Z),
  related(Z,Y).

get_grandchild :-
  parent(albert,X),
  parent(X,Y),
  write('albert grandchild is '),
  write(Y),nl.

get_grandparent :-
  parent(X,carl),
  parent(X,charlie),
  format('~w ~s grandparent ~n',[X,"is the"]).
  
brother(bob,bill).

grand_parent(X,Y) :-
  parent(Z,X),
  parent(Y,Z).

city(X) :- country(X).
country(usa).

happy(albert).
happy(alice).
happy(bob).
happy(bill).
with_albert(alice).

runs(albert) :-
  happy(albert).
  
dances(alice) :-
  happy(alice),
  with_albert(alice).
  
does_alice_dance :- dances(alice),
  write('when she is happy and with albert').
  
swims(bill) :-
  happy(bill).

swims(bill) :-
  near_water(bill).



what_grade(5) :-
  write('go to kindergarten').
  
what_grade(6) :-
  write('go to 1st grade').

what_grade(Other) :-
  Grade is Other-5,
  format('go to grade ~w',[Grade]).

has(albert,olive).
owns(albert,pet(cat,olive)).

customer(tom,smith,20).
customer(sally,smith,40).

get_customer_balance(Firstname,Lastname) :-
  customer(Firstname,Lastname,Balance),
  write(Firstname),tab(1),
  format('~w owes us $~2f ~n',[Lastname,Balance]).

vertical(line(point(X,Y),point(X,Y2))).
horizontal(line(point(X,Y),point(X2,Y))).

warm_blooded(penguin).
warm_blooded(human).

produce_milk(penguin).
produce_milk(human).

have_feathers(penguin).
have_hair(human).

mammal(X) :-
  warm_blooded(X),
  produce_milk(X),
  have_hair(X).
  
double_digit(X,Y) :-
  Y is X*2.

is_even(X) :-
  Y is X//2, X =:= 2*Y.

say_hi :-
  write('what is your name ?'),
  read(X),
  write('Hi '),
  write(X).

fav_char :-
  write('fav char ?'),
  get(X),
  format('the ascii value ~w is ',[X]),
  put(X),nl.

write_to_file(File,Text) :-
  open(File,write,Stream),
  write(Stream,Text),nl,
  close(Stream).

read_file(File) :-
  open(File,read,Stream),
  get_char(Stream,Char1),
  process_stream(Char1,Stream),
  close(Stream).

process_stream(end_of_file,_) :- !.

process_stream(Char,Stream) :-
  write(Char),
  get_char(Stream, Char2),
  process_stream(Char2,Stream).


count_to_10(10) :- write(10),nl.

count_to_10(X) :-
  write(X),nl,
  Y is X + 1,
  count_to_10(Y).

count_down(Low,High) :-
  between(Low,High,Y),
  Z is High - Y,
  write(Z),nl.
  

guess_num :- loop(start).

loop(15) :- write('right!').

loop(X) :-
  X \= 15,
  write('guess number '),
  read(Guess),
  write(Guess),
  write(' is not the number'),nl,
  loop(Guess).


write_list([]).

write_list([Head|Tail]) :-
  write(Head),nl,
  write_list(Tail).


join_str(Str1,Str2,Str3) :-
  name(Str1, StrList1),
  name(Str2, StrList2),
  append(StrList1,StrList2,StrList3),
  name(Str3,StrList3).







