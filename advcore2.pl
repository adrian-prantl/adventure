% Adventure core 2nd edition
% (C) 2007-2010 Adrian Prantl
:- module(advcore2, [main/1,new_game/2,action/3,sentence/4,word/2]).
:- use_module(library(assoc)).

:- use_module('contrib/wordnet/wn_s').
:- discontiguous(action/1).

% Quit on compile-time error
user:message_hook(_Term, error, Lines) :-
  %member(WE, [warning,error]),
  print_message_lines(user_error, 'ERROR: ', Lines),
  halt(1).

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
  format_xy('Words = ~w, Cs = ~w~n', [WordsR,CsR], 0, 23),
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
   %cwrite('>'),
   %io_loop(WsR, CsR)
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


action(S, S1, [Action|Params]) :-
  % this is basically a security measure, st. nobody can use something
  % like `shell'
  action(Action),

  % call the action
  Goal =.. [Action|[S|[S1|Params]]],
  catch(Goal, E, error(Goal, E, S, S1)).

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
% Meta-macro system
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

declare1(S, new_object(Name, LongName), S) :-
  declare1(S, new_object(Name, LongName, 'it has nothing special about it', []), S).

declare1(S, new_object(Name, LongName, Desc), S) :-
  declare1(S, new_object(Name, LongName, Desc, []), S).

declare1(S, new_object(Name, LongName, Desc, Attrs), S1) :-
  % this is now a static rule:
  % asserta(( object(State, Name) :- here(State, Name)) ),
  maplist(wrapped(Name), Attrs, Attrs1),
  foldl(Attrs1, declare_attr, S, S1),
  % Set the Description
  asserta(long_name(Name, LongName)),
  asserta(description(Name, Desc)).

declare1(S, new_location(Room, Desc, Doors), S2) :-
  declare1(S, new_location(Room, Desc, Doors, []), S2) .
declare1(S, new_location(Room, Desc, Doors, Objects), S2) :-
  % @todo only visited locations and direct doors
  asserta(location(_,Room)),
  asserta(description(Room, Desc)),
  new_xs(S, door(Room), Doors, S1),
  new_objects(S1, Room, Objects, S2).
declare1(S, new_person(Name, Desc), S) :-
  asserta(person(_,Name)),
  asserta(description(Name, Desc)).
declare1(S, new_inside(Place, X), S1) :-
  put_assoc(inside(X), S, Place, S1).
declare1(S, new_on(Place, X), S1) :-
  put_assoc(on(X), S, Place, S1).

wrapped(Arg, Functor, Term) :-
  Term =.. [Functor, Arg].

declare_attr(S, Attribute, S1) :-
  put_assoc(Attribute, S, true, S1).

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

%-----------------------------------------------
% BASIC GAME MECHANICS

% grammar structure
noun_type(object).
noun_type(location).
noun_type(person).

new_game(GameDescription-StartLocation, NewGame) :-
  list_to_assoc([here-StartLocation], S),
  maplist(retractall, [long_name, description, person, location]),
  % nouns
  declare(S, GameDescription, NewGame),
  % Now seal off the Prolog engine
  retractall(assert),
  retractall(asserta),
  retractall(assertz).

% properties
inflammable(wood).
bibulous(wood).

material(door, wood).

% helper predicates


%% carrying(+State, ?Obj)
carrying(S, Obj) :- gen_assoc(inside(Obj), S, inventory).

%% inside_of(+State, ?Obj, ?ContainerOrRoom)
inside_of(S, Obj, ContainerOrRoom) :- gen_assoc(inside(Obj), S, ContainerOrRoom).

%% inside_of(+State, ?Obj, ?Object)
on(S, Obj, Object) :- gen_assoc(on(Obj), S, Object).

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
  get_assoc(can_be_opened(Obj), S, true).
% An object that holds other objects
container_object(S, Obj) :-
  object(S, Obj),
  inside_of(S, _, Obj).

% actions
%--------------------------------------------------------------------

% look
action(look).
look(S, S) :-
  get_assoc(here, S, Location),
  printable(Location, L),
  description(Location, Desc),
  answer('You are in the ~~location(~w).~n~w', [L, Desc]),
  look_objects(S, Location, L),
  look_doors(S, Location).

% List all objects via backtracking
look_objects(S, Location, L) :-
  inside_of(S, Obj, Location),
  printable(Obj, ObjName),
  % if it can be opened, it must be open
  (openable_object(S, Location)
  ->  get_assoc(open(Location), S, true)
  ; answer('The ~w is closed shut.', [ObjName])),
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
  (  get_assoc(open(Obj), S, true)
  -> printable(Obj, O),
     look_objects(S, Obj, O)
  ;  true).

look_doors(S, Location) :-
  get_assoc(door(Location), S, Room),
  printable(Room, R),
  answer('From here you can go to the ~w.', [R]),
  fail.
look_doors(_, _).

% look at
action(look_at).
look_at(S, S, X) :-
  description(X, Desc),
  answer('~w~n', [Desc]),
  look_objects(S, X, X). % fixme

% look in
action(look_in).
look_in(S, S, X) :-
  look_objects(S, X, X).

% open
%:- retractall(open(_,_,_)). % clashes with file i/o predicate otherwise
action(open_).
open_(S, S1, Obj) :-
  printable(Obj, ObjName),
  (  get_assoc(can_be_opened(Obj), S, true)
  ->
     put_assoc(open(Obj), S, true, S1),
     answer('Behold, the ~w is now open.~n', [ObjName]),
     look_objects(S1, Obj, ObjName)
  ;
     answer('You cannot open the ~w!~n', [ObjName])
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
     atomic_list_concat(Path1, ', ', PathL),
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
  -> answer1(ObjName),
     nb_setarg(1, First, 1)
  ;  answer1(', ~w', [ObjName])
  ),
  fail.
list_inventory(_) :- answer('.').


%-----------------------------------------------
% GRAMMAR

% Begin - reverse rules
word(_,W) :- phrase(det, Ws),				   member(W, Ws).
word(_,W) :- phrase(pers_det, Ws),			   member(W, Ws).
word(_,W) :- phrase(trans_verb(_,_), Ws),		   member(W, Ws).
word(_,W) :- phrase(intrans_verb(_), Ws),		   member(W, Ws).
word(_,W) :- phrase(adverb(_), Ws),			   member(W, Ws).
word(S,W) :- noun_type(T), phrase(noun(S^T,_), Ws),        member(W, Ws).
word(_,W) :- noun_type(T), phrase(preposition(T,_), Ws), member(W, Ws).
% End - reverse rules

sentence([Verb],_) --> intrans_verb(Verb).
sentence([Verb, Noun],S) --> trans_verb(Type, Verb), preposition(Type,_), nounphrase(S^Type, Noun).
sentence([Verb, Noun],S) --> trans_verb(Type, Verb), nounphrase(S^Type, Noun).

det --> [the].
%det --> [a].
%det --> [an].
pers_det --> [my].

nounphrase(S^Type,Noun) --> { carrying(S, Noun) }, pers_det, noun(S^Type,Noun).
nounphrase(Type,Noun) --> det,noun(Type,Noun).
%nounphrase(Type,Noun) --> noun(Type,Noun).

% This is the bi-directional definition of noun/3
noun(S^Type, Noun) --> { call(Type,S,Noun), atom(Noun) }, [Noun].
noun(S^Type, Noun) --> { call(Type,S,Noun), is_list(Noun) }, Noun.

verb(Type,V) --> trans_verb(Type,V).
verb(intrans,V) --> intrans_verb(V).
intrans(_) :- fail.

% use key on door
% open door
% take anvil?
% use saw with bread -> key

%preposition(object,at) --> [at].
preposition(container_object,in) --> [in].
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

trans_verb(object, take) --> { synonym_of([202205272, 200173338], Take) }, Take.
trans_verb(object, drop) --> [drop].
trans_verb(object, eat) --> [eat].
trans_verb(object, turn_on) --> { synonym_of([201510399], TurnOn) }, TurnOn.
trans_verb(object, turn_off) --> { synonym_of([201510576], TurnOff) }, TurnOff.
trans_verb(person, talk_to) --> [talk,to].
trans_verb(container_object, look_in) --> [look,in].
trans_verb(object, look_at) --> { synonym_of([202130300,202131777], LookAt) }, LookAt.
trans_verb(person, look_at) --> { synonym_of([202130300,202131777], LookAt) }, LookAt.
trans_verb(openable_object, open_) --> [open].
trans_verb(location, go) --> [go].
trans_verb(location, go) --> [enter].
trans_verb(location, go) --> [walk].

intrans_verb(inventory) --> [inventory].
intrans_verb(look) --> [look].
intrans_verb(quit) --> { synonym_of([202680814, 106629610], Quit) }, Quit.
intrans_verb(quit) --> [q].
