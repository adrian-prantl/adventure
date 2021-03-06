% Adventure core 2nd edition
%
% (C) 2007-2012 Adrian Prantl
% This file is part of Adventure.
%
% Adventure is free software: you can redistribute it and/or modify
% it under the terms of the GNU Affero General Public License as
% published by the Free Software Foundation, either version 3 of the
% License, or (at your option) any later version.
%
% Adventure is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Affero General Public License for more details.
%
% You should have received a copy of the GNU Affero General Public
% License along with Adventure.  If not, see
% <http://www.gnu.org/licenses/>.
%
:- module(advcore2, [main/1,new_game/2,action/3,sentence/4,word/2]).
:- use_module(library(assoc)).

:- use_module('contrib/wordnet/wn_s').
:- discontiguous(action/1).

% Quit on compile-time error
user:message_hook(_Term, error, Lines) :-
  %member(WE, [warning,error]),
  print_message_lines(user_error, 'ERROR: ', Lines),
  halt(1).

quiet. % comment this for verbose debug output.
quiet :- fail.

main(Name)  :-
  open(Name, read, File, []),
  read_term(File, (Title:Game), [syntax_errors(fail)]),
  close(File),
  format('~w~n~n', [Title]),
  answer('Welcome!~n'), !,
  new_game(Game, State), !,
  look(State, S1),
  new_command(S1).

bye :- cformat('~nGoodbye!~n', []).

cwrite(T) :- write_xy(T, -1, -1).
cformat(Format, Arguments) :-
  format(atom(T), Format, Arguments),
  cwrite(T).
format_xy(Format, Arguments, X, Y) :-
  format(atom(T), Format, Arguments),
  write_xy(T, X, Y).

new_command(S) :-
  prompt,
  compound(S), % Type checking
  io_loop(S, [],[]).

% Display the prompt and optionally redraw the user input
prompt :- bold, cwrite('\r>'), roman.
prompt(WsR, CsR) :-
  prompt,
  lists_sentence(WsR, CsR, Sentence),
  maplist(cwrite_, Sentence).

%%io_loop(+S, +WordsR, +CsR)
% The main I/O loop.
%
% * S is the current state.
%
% * WordsR is the reversed list of words read so far.
%
% * CsR is the reversed list of characters in the word the user is
%   typing at the moment.
io_loop(S, WordsR, CsR) :-
  (quiet ; format_xy('Words = ~w, Cs = ~w~n', [WordsR,CsR], 0, 23)),
  autocomplete(S, WordsR, CsR),
  getch(Char), !,
  %format_xy('Char = ~c (~w)~n', [Char,Char], 64, 0),
  handle_char(S, Char, WordsR, CsR).

% Switch over current Char
handle_char(S, C, WsR, CsR) :-
  char_type(C, alnum), atom_codes(Ch, [C]), cwrite(Ch),
  io_loop(S, WsR, [C|CsR]).
handle_char(S, C, WsR, CsR) :-
  char_code(' ', C), cwrite(' '),
  handle_whitespace(S, WsR, CsR).
handle_char(S, C, WsR, CsR) :- char_code('\t', C), handle_tabulator(S, WsR, CsR).
handle_char(S, C, WsR, CsR) :- backspace(C),       handle_backspace(S, WsR, CsR).
handle_char(_, C,_WsR,_CsR) :- char_type(C, end_of_file), bye.
handle_char(S, C, WsR, CsR) :- char_type(C, end_of_line),
  lists_sentence(WsR, CsR, Sentence),
  (phrase(sentence(A,S), Sentence)
  -> cwrite('\n'),
   action(S, S1, A),
   new_command(S1)
  ; prompt(WsR, CsR),
   italic, cwrite('\nSorry, I could not understand that!\n'), roman,
   new_command(S)
  ).
handle_char(S, _, WsR, CsR) :- % ignore
  prompt(WsR, CsR),
  io_loop(S, WsR, CsR).

lists_sentence(WordsR, CharsR, Sentence) :-
  reverse(CharsR, Cs),
  atom_codes(W, Cs),
  reverse(Sentence, [W|WordsR]).

%-----------------------------------------------
handle_whitespace(S, WordsR, []) :- !,
  % Ignore Whitespace
  io_loop(S, WordsR, []).
handle_whitespace(S, WordsR, CsR) :-
  reverse(CsR, Cs), % finish this word
  atom_codes(W, Cs),
  io_loop(S, [W|WordsR], []).

handle_tabulator(S, WordsR, CsR) :-
  % ignore tab char, autocomplete current word
  (finishes_sentence(S, WordsR, CsR, Suffix, _AC_Words)
  -> ( cwrite(Suffix),
       atom_codes(Suffix, Codes),
       reverse(Codes, SuffixR),
       append(SuffixR, CsR, CompletedWord),
       io_loop(S, WordsR, CompletedWord)
     )
  ; io_loop(S, WordsR, CsR)).

%-----------------------------------------------
% Handle Backspace
backspace(127).
backspace(8).

handle_backspace(S, [], []) :- !,
  io_loop(S, [], []).
handle_backspace(S, [W|WordsR], []) :- !,
  cwrite('\b \b\b \b'), % get rid of that extra space
  atom_codes(W, Cs),
  reverse(Cs, [_|CsR]),
  io_loop(S, WordsR, CsR).
handle_backspace(S, WordsR, [_|CsR]) :-
  cwrite('\b \b'),
  io_loop(S, WordsR, CsR).

%-----------------------------------------------
% Autocompletion with immediate I/O

finishes_sentence(S, WordsR, CsR, Suffix, Words) :-
  reverse(Cs, CsR),
  atom_codes(W1, Cs),
  % Append Letters
  word(S,W2),
  atom_concat(W1, Suffix, W2),
  % ... and words
  reverse(Ws, [W2|WordsR]),
  append(Ws, Words, CsX),
  % and find an autocompletion
  phrase(sentence(_,S), CsX).
finishes_sentence(_, _, _, ' ', []) :- repeat.

autocomplete(_, [], []). % Do not autocomplete an empty sentence
autocomplete(S, WordsR, CsR) :-
  %cwrite('\b\b\b\b\b['),
  recorda(count, 0),

  finishes_sentence(S, WordsR, CsR, Suffix, AC_Words),
    % Console output
    once(recorded(count, N)),
    % writeln(N),
    Line is 1 + N,
    append([[Suffix], AC_Words, ['.','','','']], T),
    format_xy('~w ~w ~w ~w ~w~n', T, 40, Line),
    N1 is N+1, recorda(count, N1),
  (N >= 3 -> true; fail),
  format_xy('$~n', [], 40, N1).

  %cwrite(']\n'),
  %print_line(WordsR, CsR),
  %io_loop(WordsR, CsR).
autocomplete(_, _) :- cwrite('ERROR'), halt.

cwrite_(X) :- cwrite(' '), cwrite(X).

print_line(WordsR, CsR) :-
  reverse(Cs, CsR),
  atom_codes(W, Cs),

  reverse(Words, WordsR),
  maplist(cwrite_, Words),
  cwrite(W).


action(S, S3, [Action|Params]) :-
  % This is basically a security measure to prevent users from
  % entering commands like `shell'.
  action(Action),

  % call the precondition, if any.
  Goal1 =.. [Action|[S|[S1|Params]]],
  before(Goal1), !,

  % call the action
  Goal2 =.. [Action|[S1|[S2|Params]]],
  catch(Goal2, E, error(Goal2, E, S1, S2)),
  Goal3 =.. [Action|[S2|[S3|Params]]],
  after(Goal3).
action(S, S, _) :- answer('Better luck next time!').

error(Goal, E, S, S) :-
  writeln(E),
  Goal =.. [F|_],
  answer('Sorry, I don\'t know how to ~w properly.~n', [F]).

change(Key, Term) :-
  (recorded(Key, Term, Reference)
  -> erase(Reference)
  ; true),
  recorda(Key, Term).

answer(Message)     :- italic, cformat(Message, []), cwrite('\n'), roman.
answer(Message, Xs) :- italic, cformat(Message, Xs), cwrite('\n'), roman.
answer1(Message)    :- italic, cformat(Message, []), roman.
answer1(Message,Xs) :- italic, cformat(Message, Xs), roman.

%-----------------------------------------------
% Meta-macro system / Game descriptions.
%-----------------------------------------------

%% foldl(?List, ?Pred, ?Start, ?Result)
% Fold a list left-to-right using Pred, just as you would do in Haskell.
% ==
% pred(LHS, RHS, Result)
% ==
% Thanks to Markus Triska for the definition.
foldl([], _, Result, Result).
foldl(List, Pred, Start, Result) :-
  fold_lag(List, Start, Pred, Result).

fold_lag([], Result, _, Result).
fold_lag([RHS|Xs], LHS, Pred, Result) :-
  call(Pred, LHS, RHS, Accum),
  fold_lag(Xs, Accum, Pred, Result).

% This is a meta-predicate to save the author repetitive typing
% Use it to associate Things with Descriptions
declare(S, Xs, S1) :-
  foldl(Xs, declare1, S, S1).
%  phrase(declare1(Xs), S, S1).

declare1(S, object(Name, LongName), S) :-
  declare1(S, object(Name, LongName, 'it has nothing special about it', []), S).

declare1(S, object(Name, LongName, Desc), S) :-
  declare1(S, object(Name, LongName, Desc, []), S).

declare1(S, object(Name, LongName, Desc, Attrs), S1) :-
  % this is now a static rule:
  % asserta(( object(State, Name) :- here(State, Name)) ),
  maplist(wrapped(Name), Attrs, Attrs1),
  foldl(Attrs1, declare_attr, S, S1),
  % Set the Description
  asserta(long_name(Name, LongName)),
  asserta(description(_, Name, Desc)).

declare1(S, location(Room, Desc, Doors), S2) :-
  declare1(S, location(Room, Desc, Doors, []), S2) .
declare1(S, location(Room, Desc, Doors, Objects), S2) :-
  % @todo only visited locations and direct doors
  asserta(location(_,Room)),
  asserta(description(_, Room, Desc)),
  new_xs(S, door(Room), Doors, S1),
  new_objects(S1, Room, Objects, S2).
declare1(S, person(Name, Desc), S) :-
  asserta(person(_,Name)),
  asserta(description(_, Name, Desc)).
declare1(S, inside(Place, X), S1) :-
  put_assoc(inside(X), S, Place, S1).
declare1(S, on(Place, X), S1) :-
  put_assoc(on(X), S, Place, S1).

% A conditional description is executed before the generic description
% and then fails, thus executing the generic description.
declare1(S, description_if(Object, Conditions, Desc), S) :-
  condition_expr(State, Object, Conditions, CondExpr),
  asserta(description(State, Object, _) :-
	    (CondExpr, answer(Desc), fail)).

% This is where most of the game logic is implemented.
declare1(S, trigger(Obj, DCG), S) :-
  expand_term(trigger(Obj) --> DCG, Expanded),
  asserta(Expanded).

% FIXME. We should have a proper syntax instead of using assert.
declare1(S, assert(Rule), S) :-
  asserta(Rule).

declare1(S, Malformed, S) :- gtrace,
  answer('ERROR: malformed rule ~w.', Malformed).

% Build a comma separated list of conditions.
condition_expr(_, _, [], true).
condition_expr(S, Obj, [not(C)|Conditions], (\+ is(Obj, C, S, S), CExprs)) :-
  condition_expr(S, Obj, Conditions, CExprs).
condition_expr(S, Obj, [C|Conditions], (is(Obj, C, S, S), CExprs)) :-
  condition_expr(S, Obj, Conditions, CExprs).
condition_expr(S, Obj, NoList, Expr) :-
  condition_expr(S, Obj, [NoList], Expr).

wrapped(Arg, Functor, Term) :-
  Term =.. [Functor, Arg].

declare_attr(S, Attribute, S1) :-
  set(Attribute, S, S1).

new_object(S, Room, Object, S1) :- put_assoc(inside(Object), S, Room, S1).

new_xs(S, _, [], S).
new_xs(S, Key, [X|Xs], S2) :-
  put_assoc(Key, S, X, S1),
  new_xs(S1, Key, Xs, S2).

new_objects(S, _, [], S).
new_objects(S, Room, [Object|Objects], S2) :-
  new_object(S, Room, Object, S1),
  new_objects(S1, Room, Objects, S2).

printable([X|Xs], A) :- atomic_list_concat([X|Xs], ' ', A), !.
printable(A, A) :- atom(A).

%% delete_assoc(?Key, +Assoc, ?NewAssoc)
% quite slow.
delete_assoc(Key, Assoc, NewAssoc) :-
  assoc_to_list(Assoc, List),
  delete(List, Key-_, List2),
  list_to_assoc(List2, NewAssoc).

% Shortcut for querying attributes from the state assoc data structure.
is(Object, Attribute, State, State) :-
  Goal =.. [Attribute, Object],
  get_assoc(Goal, State, true).

% Not is/4.
is_not(Object, Attribute, State, State) :-
  \+ is(Object, Attribute, State, State).

% DCG-friendly version of put_assoc/4.
set(Attribute, S, S1) :-
  put_assoc(Attribute, S, true, S1).
set(Attribute, Value, S, S1) :-
  put_assoc(Attribute, S, Value, S1).

replace(OldObj, NewObj) -->
  inside_of(OldObj, Loc),
  delete_assoc(inside(OldObj)),
  set(inside(NewObj), Loc).

  

%-----------------------------------------------
% BASIC GAME MECHANICS

% grammar structure
noun_type(object).
noun_type(location).
noun_type(person).

new_game(GameDescription-StartLocation, NewGame) :-
  list_to_assoc([here-StartLocation], S),
  maplist(retractall, [long_name, description, person, location, before, after, trigger]),

  % default pre/postconditions.
  assertz(before(Action) :- (Action =.. [_|[S1|[S1|_]]])),
  assertz(after(Action)  :- (Action =.. [_|[S2|[S2|_]]])),
  assertz(trigger(_, S3, S3)),

  % Load the game.
  declare(S, GameDescription, NewGame),
  % Now seal off the Prolog engine
  retractall(assert),
  retractall(asserta),
  retractall(assertz).

new_game(_, []) :- gtrace,
  answer('There was an error loading the game.').

% properties
inflammable(wood).
bibulous(wood).

material(door, wood).

% helper predicates


%% carrying(+State, ?Obj)
carrying(S, Obj) :- gen_assoc(inside(Obj), S, inventory).

%% inside_of(?Obj, ?ContainerOrRoom, +State, -State)
inside_of(Obj, ContainerOrRoom, S, S) :-
  gen_assoc(inside(Obj), S, ContainerOrRoom).

%% on(+State, ?Obj, ?Object)
on(S, Obj, Object) :- gen_assoc(on(Obj), S, Object).

%% next_to(?Obj, ?ContainerOrRoom, +State, -State1)
% Two objects are next to each other if they are in the same container.
next_to(ObjA, ObjB, S, S) :-
  gen_assoc(inside(ObjA), S, ContainerOrRoom),
  ContainerOrRoom \= inventory,
  gen_assoc(inside(ObjB), S, ContainerOrRoom).


%% here(+State, ?Obj)
% An object is here if it is in the inventory, the current room or
% inside an open object that is here.
here(S, Obj) :- carrying(S, Obj).
here(S, Obj) :- get_assoc(here, S, Here), gen_assoc(inside(Obj), S, Here).
here(S, Obj) :-
  gen_assoc(inside(Obj), S, Container),
  gen_assoc(open(Container), S, true),
  here(S, Container).
here(S, Obj) :- 
  gen_assoc(on(Obj), S, Object),
  here(S, Object).

% Static grammar rules
object(S, Obj) :- here(S, Obj).

% An object is only part of the grammar if it is inside the current
% room or in the inventory.
openable_object(S, Obj) :-
  object(S, Obj),
  is(Obj, can_be_opened, S, S).
% An object that holds other objects
container_object(S, Obj) :-
  object(S, Obj),
  is(Obj, container, S, S).

% actions
%--------------------------------------------------------------------

% look
action(look).
look(S, S) :-
  get_assoc(here, S, Location),
  printable(Location, L),
  description(S, Location, Desc),
  answer('You are in the ~~location(~w).~n~w', [L, Desc]),
  look_objects(S, Location, L),
  look_doors(S, Location).

% List all objects via backtracking
look_objects(S, Location, L) :-
  % for each Obj at Location
  inside_of(Obj, Location, S, S),
  printable(Obj, ObjName),
  % if it can be opened, it must be open to continue
  (openable_object(S, Location)
  ->  (is(Location, open, S, S)
      -> true
      ;  answer('The ~w is closed shut.', [Location]),
         fail)
  ; true),
  answer('Inside the ~w there is a ~~object(~w).', [L, ObjName]),
  look_inside_objects(S, Obj),
  fail.
look_objects(S, Location, L) :-
  on(S, Obj, Location),
  printable(Obj, ObjName),
  answer('On the ~w there is a ~w.', [L, ObjName]),
  fail.
look_objects(_, _, _).

% helper for container objects
look_inside_objects(S, Obj) :-
  (  is(Obj, open, S, S)
  -> printable(Obj, O),
     look_objects(S, Obj, O)
  ;  true).

look_doors(S, Location) :-
  get_assoc(door(Location), S, Room),
  printable(Room, R),
  answer('From here you can go to the ~~location(~w).', [R]),
  fail.
look_doors(_, _).

% look at
action(look_at).
look_at(S, S, X) :-
  description(S, X, Desc),
  answer('~w~n', [Desc]),
  look_objects(S, X, X). % fixme

% look in
action(look_in).
look_in(S, S, X) :-
  look_objects(S, X, X).

% open
% Named open_ to prevent conflict with the file i/o predicate.
action(open_).
open_(S, S1, Obj) :-
  printable(Obj, ObjName),
  (  is(Obj, can_be_opened, S, S)
  ->
     set(open(Obj), S, S1),
     answer('Behold, the ~w is now open.~n', [ObjName]),
     look_objects(S1, Obj, ObjName)
  ;
     answer('You cannot open the ~w!~n', [ObjName])
  ).

% close
% Named close_ to prevent conflict with the file i/o predicate.
action(close_).
close_(S, S1, Obj) :-
  printable(Obj, ObjName),
  (  is(Obj, open, S, S)
  ->
     delete_assoc(open(Obj), S, S1),
     answer('You close the ~w.~n', [ObjName])
  ;
     answer('You cannot close the ~w!~n', [ObjName])
  ).


% go
% FIXME this will fail with circular room layouts
action(go).
path_to(S, A, B, Path) :-
  path_to(S, A, B, [], P_rev),
  reverse(Path, P_rev).
path_to(_, Here, Here, P, P).
path_to(S, Here, Location, P, Path) :-
  get_assoc(door(Here), S, There),
  path_to(S, There, Location, [There|P], Path).

go(S, S, Location) :-
  \+ location(S, Location), !,
  printable(Location, L),
  answer('Sorry, ~w is not a place we can go to.', [L]).
go(S, S2, Location) :-
  ( get_assoc(here, S, Here),
    path_to(S, Here, Location, Path),
    go(S, S2, Location, Path) )
  ; S = S2,
  printable(Location, L),
  answer('Sorry, there is no way we can reach ~w at the moment.', [L]).
go(S, S, Location, []) :- !,
  printable(Location, L),
  answer('You are already in the ~w.', [L]).
go(S, S2, Location, Path) :- !,
  put_assoc(here, S, Location, S1),
  printable(Location, L),
  ( length(Path, 1)
  -> answer('You are entering the ~w.', [L])
  ;  append(Path1, [_], Path), % skip the destination
     maplist(printable, Path1, Path2),
     atomic_list_concat(Path2, ', ', PathL),
     answer('Passing through ~w you move towards the ~w.', [PathL, L])
  ),
  look(S1, S2).

% take
action(take).
take(S, S2, Object) :- %trace,
  ( object(S, Object) % Weight, inventory(S, _, Weight)
  -> ( here(S, Object)
       -> ( put_assoc(inside(Object), S, inventory, S1),
	      delete_assoc(on(Object), S1, S2),
	    answer('You now have the ~w.', [Object]) )
       ; answer('There is no ~w within sight.', [Object])
     )
  ; answer('The ~w is not something you can take with you.', [Object])
  ).

% put
action(put).
put(S, S1, DirectObj, IndirectObj) :-
  carrying(S, DirectObj), !,
  put_object(S, S1, DirectObj, IndirectObj).
put(S, S, DirectObj, _) :-
  answer('You do not carry a ~w.', [DirectObj]).

put_object(S, S3, DirectObj, IndirectObj) :-
  here(S, IndirectObj),
  container_object(S, IndirectObj), !,
  delete_assoc(inside(DirectObj), S, S1),
  put_assoc(inside(DirectObj), S1, IndirectObj, S2),
  answer('You place the ~w inside the ~w.', [DirectObj, IndirectObj]),
  trigger(DirectObj, S2, S3).
put_object(S, S3, DirectObj, IndirectObj) :-
  here(S, IndirectObj),
  location(S, IndirectObj), !,
  delete_assoc(inside(DirectObj), S, S1),
  put_assoc(on(DirectObj), S1, IndirectObj, S2),
  answer('You leave the ~w in the ~w.', [DirectObj, IndirectObj]),
  trigger(DirectObj, S2, S3).
put_object(S, S, _, _) :-
  answer('Sorry, but you cannot do that.').

% turn on.
action(turn_on).
turn_on(S, S1, Object) :-
  object(S, Object), !,
  turn_on1(S, S1, Object).
turn_on(S, S, Object) :-
  answer('There is no ~w here that you could turn on.', [Object]).

turn_on1(S, S, Object) :-
  is(Object, turned_on, S, S), !,
  answer('The ~w is already on.', [Object]).

turn_on1(S, S3, Object) :-
  is(Object, turned_off, S, S), !,
  set(turned_on(Object), S, S1),
  delete_assoc(turned_off(Object), S1, S2),
  answer('You turn on the ~w.', [Object]),
  trigger(Object, S2, S3).

% turn off.
action(turn_off).
turn_off(S, S1, Object) :-
  object(S, Object), !,
  turn_off1(S, S1, Object).
turn_off(S, S, Object) :-
  answer('There is no ~w around here, and it isn\'t on either.', [Object]).

turn_off1(S, S, Object) :-
  is(Object, turned_off, S, S), !,
  answer('The ~w is already off.', [Object]).

turn_off1(S, S3, Object) :-
  is(Object, turned_on, S, S), !,
  set(turned_off(Object), S, S1),
  delete_assoc(turned_on(Object), S1, S2),
  trigger(Object, S2, S3),
  answer('You turn the ~w off.', [Object]).

% quit
action(quit).
quit(S, S) :- bye.

% inventory
action(inventory).
inventory(S, S) :-
  answer1('You are carrying '),
  ( list_inventory(S)
  ; answer('nothing at all.')).

% List all objects via backtracking
list_inventory(S) :-
  First = counter(0),
  carrying(S, Obj),
  printable(Obj, ObjName),
  (  First = counter(0)
  -> answer1('a ~w', ObjName),
     nb_setarg(1, First, 1)
  ;  answer1(', a ~w', [ObjName])
  ),
  fail.
list_inventory(_) :- answer('.').


%-----------------------------------------------
% GRAMMAR

% The following clauses are logically redundant, but necessary for
% pruning the search tree when traversing the grammar backwards.
% Begin - reverse rules
word(_,W) :- phrase(det, Ws),				   member(W, Ws).
word(_,W) :- phrase(pers_det, Ws),			   member(W, Ws).
word(_,W) :- phrase(trans_verb(_,_), Ws),		   member(W, Ws).
word(_,W) :- phrase(ditrans_verb(_,_,_), Ws),		   member(W, Ws).
word(_,W) :- phrase(intrans_verb(_), Ws),		   member(W, Ws).
word(_,W) :- phrase(adverb(_), Ws),			   member(W, Ws).
word(S,W) :- noun_type(T), phrase(noun(S^T,_), Ws),        member(W, Ws).
word(_,W) :- noun_type(T), phrase(preposition(T,_), Ws), member(W, Ws).
% End - reverse rules

sentence([Verb],_) --> intrans_verb(Verb).
sentence([Verb, Noun1, Noun2],S) -->
  ditrans_verb(Verb, DirectTy, IndirectTy),
  nounphrase(S^DirectTy, Noun1),
  preposition(IndirectTy, _),
  nounphrase(S^IndirectTy, Noun2).
sentence([Verb, Noun],S) -->
  trans_verb(Verb, Type), preposition(Type,_), nounphrase(S^Type, Noun).
sentence([Verb, Noun],S) -->
  trans_verb(Verb, Type), nounphrase(S^Type, Noun).

det --> [the].
%det --> [a].
%det --> [an].
pers_det --> [my].

nounphrase(S^Type, Noun) --> { carrying(S, Noun) }, pers_det, noun(S^Type, Noun).
nounphrase(S^Type, Noun) --> det, noun(S^Type, Noun).
%nounphrase(Type, Noun) --> noun(Type,Noun).

% This is a bi-directional definition of noun/3.
noun(S^Type, Noun) --> { call(Type,S,Noun), atom(Noun) }, [Noun].
noun(S^Type, Noun) --> { call(Type,S,Noun), is_list(Noun) }, Noun.

%verb(Type,V) --> ditrans_verb(V, _, Type).
%verb(Type,V) --> trans_verb(V, Type).
%verb(intrans,V) --> intrans_verb(V).
%intrans(_) :- fail.

% use key on door
% open door
% take anvil?
% use saw with bread -> key

%preposition(object,at) --> [at].
preposition(container_object,in) --> [in].
preposition(container_object,in) --> [into].
%preposition(object,on) --> [on].
preposition(location,to) --> [to].
preposition(location,to) --> [into].
preposition(location,to) --> [inside].
preposition(location,to) --> [towards].

adverb(on) --> [on].
adverb(off) --> [off].

% use s(S, _, 'word', _, _, _), s(S, _, Synonym, _, _, _), g(S, Gloss). to get the syn_id for a word.
synonym_of(SynIds, Synonym) :-
  member(SynId, SynIds),
  s(SynId, _, S, _, _, _),
  atomic_list_concat(Synonym, ' ', S).

ditrans_verb(put, object, container_object) --> { synonym_of([201494310], Put) }, Put.

trans_verb(take, object) --> { synonym_of([202205272, 200173338], Take) }, Take.
trans_verb(eat, object) --> [eat].
trans_verb(turn_on, object) --> { synonym_of([201510399], TurnOn) }, TurnOn.
trans_verb(turn_off, object) --> { synonym_of([201510576], TurnOff) }, TurnOff.
trans_verb(talk_to, person) --> [talk,to].
trans_verb(look_in, container_object) --> [look,in].
trans_verb(look_at, object) --> { synonym_of([202130300,202131777], LookAt) }, LookAt.
trans_verb(look_at, person) --> { synonym_of([202130300,202131777], LookAt) }, LookAt.
trans_verb(open_, openable_object) --> [open].
trans_verb(close_, openable_object) --> [close].
trans_verb(go, location) --> [go].
trans_verb(go, location) --> [enter].
trans_verb(go, location) --> [walk].

intrans_verb(inventory) --> [inventory].
intrans_verb(look) --> [look].
intrans_verb(quit) --> { synonym_of([202680814, 106629610], Quit) }, Quit.
intrans_verb(quit) --> [q].
