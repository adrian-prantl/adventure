% Adventure core
% (C) 2007-2009 Adrian Prantl
:- module(advcore2, [main/0]).
:- use_module(library(assoc)).

main :-
  cformat('Welcome!~n', []),
  new_game(NewGame),
  new_command(NewGame).

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
  io_loop(S, [],[]).

% Display the prompt and optionally redraw the user input
prompt :- bold, cwrite('\r>'), roman.
prompt(WsR, CsR) :-
  prompt,
  lists_sentence(WsR, CsR, Sentence),
  maplist(cwrite_, Sentence).

io_loop(S, WordsR, CsR) :-
  format_xy('Words = ~w, Cs = ~w~n', [WordsR,CsR], 0, 23),
  autocomplete(S, WordsR, CsR),
  getch(Char), !,
  format_xy('Char = ~c (~w)~n', [Char,Char], 64, 0),
  handle_char(S, Char, WordsR, CsR).

% Switch over current Char
handle_char(S, C, WsR, CsR) :- char_type(C, alnum), io_loop(S, WsR, [C|CsR]).
handle_char(S, C, WsR, CsR) :- char_code(' ', C), handle_whitespace(S,WsR, CsR).
handle_char(S, C, WsR, CsR) :- char_code('\t', C), handle_tabulator(S,WsR, CsR).
handle_char(S, C, WsR, CsR) :- backspace(C),       handle_backspace(S,WsR, CsR).
handle_char(_, C,_WsR,_CsR) :- char_type(C, end_of_file), bye.
handle_char(S, C, WsR, CsR) :- char_type(C, end_of_line), 
  lists_sentence(WsR, CsR, Sentence),
  (phrase(sentence(A), Sentence)
  -> cwrite('\n'),
   action(S, S1, A),
   new_command(S1)
  ; prompt(WsR, CsR),
   italic, cwrite('\nSorry, I could not understand that!\n'), roman,
   new_command(S1)
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
  cwrite('\b'), % Ignore Whitespace
  io_loop(S, WordsR, []).
handle_whitespace(S, WordsR, CsR) :-
  reverse(CsR, Cs), % finish this word
  atom_codes(W, Cs),
  io_loop(S, [W|WordsR], []).
handle_tabulator(S, WordsR, CsR) :-
  cwrite('\b\b\b\b'), % ignore tab
  io_loop(S, WordsR, CsR).

%-----------------------------------------------
% Handle Backspace
backspace(127) :- cwrite('\b\b\b   \b\b\b').
backspace(8) :- cwrite(' \b').

handle_backspace(S, [], []) :- !,
  io_loop(S, [], []).
handle_backspace(S, [W|WordsR], []) :- !,
  cwrite('\b \b'), % get rid of that extra space
  atom_codes(W, Cs),
  reverse(Cs, [_|CsR]),
  io_loop(S, WordsR, CsR).
handle_backspace(S, WordsR, [_|CsR]) :-
  io_loop(S, WordsR, CsR).

%-----------------------------------------------
% Autocompletion

finishes_sentence(_S, WordsR, CsR, Suffix, Words) :-
  reverse(Cs, CsR),
  atom_codes(W1, Cs),
  % Append Letters
  word(W2),
  atom_concat(W1, Suffix, W2),
  % ... and words
  reverse(Ws, [W2|WordsR]),
  append(Ws, Words, CsX),
  % and find an autocompletion
  phrase(sentence(_), CsX).
finishes_sentence(_, _, _, ' ', []) :- repeat.

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
  (N >= 3 -> true; fail).

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

answer(Message) :- italic, cformat('~w~n', [Message]),  roman.
answer(Message, Xs) :- italic, cformat(Message, Xs), cwrite('\n'), roman.

% This is a meta-predicate to save the author repetitive typing
% Use it to associate Things with Descriptions
declare(S, [], S).
declare(S, [object(Name, Desc)|Xs], S1) :-
  asserta(object(Name)),
  asserta(description(Name, Desc)),
  declare(S, Xs, S1).
declare(S, [location(Room, Desc, Doors, Objects)|Xs], S2) :-
  asserta(location(Room)),
  asserta(description(Room, Desc)),
  maplist(new_door(Room), Doors),
  new_objects(S, Room, Objects, S1),
  declare(S1, Xs, S2).
declare(S, [person(Name, Desc)|Xs], S1) :-
  asserta(person(Name)),
  asserta(description(Name, Desc)),
  declare(S, Xs, S1).
new_door(A, B) :- asserta(door(A, B)).
new_object(S, Room, Object, S1) :- put_assoc(location(Object), S, Room, S1).

new_objects(S, _, [], S).
new_objects(S, Room, [Object|Objects], S2) :-
  new_object(S, Room, Object, S1),
  new_objects(S1, Room, Objects, S2).


printable([X|Xs], A) :- atomic_list_concat([X|Xs], ' ', A).
printable(A, A) :- atom(A).

%-----------------------------------------------
% BASIC GAME MECHANICS

% grammar structure
noun_type(object).
noun_type(location).
noun_type(person).

new_game(NewGame) :-
  list_to_assoc([here-'kitchen'], S), 
  % nouns
  declare(S,
[
 object(light,'It is hot.'),
 object(bread,'The bread seems extremely durable.'),
 object(lighter,'A real Zippo.'),

 location([living,room], 'A cosy living room.',[kitchen],[lighter]),
 location(kitchen,'The kitchen is small.',[[living,room]],[bread,gnome]),

 person(gnome, 'Even for a gnome he seems unusually hairy.')
],
	  NewGame).

% properties
inflammable(wood).
bibulous(wood).

material(door, wood).

% actions
%--------------------------------------------------------------------

% look
look(S, S) :-
  get_assoc(here, S, Location),
  printable(Location, L),
  description(Location, Desc),
  answer('You are in the ~w.~n~w', [L, Desc]),
  look_objects(S, Location, L),
  look_doors(Location).

% List all objects via backtracking
look_objects(S, Location, L) :-
  gen_assoc(location(Obj), S, Location),
  answer('Inside the ~w there is a ~w.', [L, Obj]),
  fail.
look_objects(_, _, _).

look_doors(Location) :- 
  door(Location, Room),
  printable(Room, R),
  answer('From here you can go to the ~w.', [R]),
  fail.
look_doors(_).

% look at
look_at(S, S, X) :-
  description(X, Desc),
  answer('~w~n', [Desc]).

% go
path_to(A, B, Path) :-
  path_to(A, B, [], P_rev),
  reverse(Path, P_rev).
path_to(Here, Here, P, P).
path_to(Here, Location, P, Path) :-
  door(Here, There),
  path_to(There, Location, [There|P], Path).

go(S, S, Location) :-
  \+ location(Location), !,
  answer('Sorry, ~w is not a place we can go to.', [Location]).
go(S, S2, Location) :-
  ( get_assoc(here, S, Here),
    path_to(Here, Location, Path),
    go(S, S2, Location, Path) )
  ; S = S2,
  answer('Sorry, there is no way we can reach ~w at the moment.', [Location]).
go(S, S, Location, []) :- !,
  answer('You are already in the ~w.', [Location]).
go(S, S2, Location, Path) :- !,
  put_assoc(here, S, Location, S1),
  ( length(Path, 1)
  -> answer('You are entering the ~w.', [Location])
  ;  append(Path1, [_], Path), % skip the destination
     atomic_list_concat(Path1, ', ', PathL),
     answer('Passing through ~w you move towards the ~w.', [PathL, Location])
  ),
  look(S1, S2).

%reachable(A, B) :- door(A, B) ; door(B, A).
%reachable(A, B) :- reachable(A, C), reachable(C, B).

%go(Location) :-
%  recorded(here, Here),
%  reachable(Here, Location),
%  change(here, Location).

% take
take(S, S1, Object) :-
  object(Object)
  -> ( get_assoc(here, S, Here),
       (get_assoc(location(Object), S, Here)
       -> (put_assoc(location(Object), S, inventory, S1),
	   answer('You now have the ~w.', [Object]))
       ; answer('There is no ~w around in the ~w.', [Object, Here])
       )
     )
  ; answer('The ~w is not something you can take with you.', [Object]).

% quit
quit(S, S) :- bye, halt.

%-----------------------------------------------
% GRAMMAR

% Begin - reverse rules
word(W) :- phrase(det, Ws),                            member(W, Ws).
word(W) :- phrase(trans_verb(_,_), Ws),                member(W, Ws).
word(W) :- phrase(intrans_verb(_), Ws),                member(W, Ws).
word(W) :- phrase(adverb(_), Ws),                      member(W, Ws).
word(W) :- noun_type(T), phrase(noun(T,_), Ws),        member(W, Ws).
word(W) :- noun_type(T), phrase(preposition(T,_), Ws), member(W, Ws).
% End - reverse rules

sentence([Verb]) --> intrans_verb(Verb).
sentence([Verb, Noun]) --> trans_verb(Type, Verb), nounphrase(Type, Noun).
sentence([Verb, Noun]) -->
  trans_verb(Type, Verb), preposition(Type,_), nounphrase(Type, Noun).

det --> [the].
det --> [my].
det --> [a].
det --> [an].

nounphrase(Type,Noun) --> det,noun(Type,Noun).
nounphrase(Type,Noun) --> noun(Type,Noun).

noun(Type, Noun) --> { call(Type,Noun), atom(Noun) }, [Noun].
noun(Type, Noun) --> { call(Type,Noun), is_list(Noun) }, Noun.

verb(Type,V) --> trans_verb(Type,V).
verb(intrans,V) --> intrans_verb(V).
intrans(_) :- fail.

preposition(object,at) --> [at].
preposition(object,in) --> [in].
preposition(location,to) --> [to].
preposition(location,to) --> [into].
preposition(location,to) --> [inside].
preposition(location,to) --> [towards].

adverb(on) --> [on].
adverb(off) --> [off].

trans_verb(object, take) --> [take].
trans_verb(object, drop) --> [drop].
trans_verb(object, eat) --> [eat].
trans_verb(object, turn_on) --> [turn,on].
trans_verb(object, turn_off) --> [turn,off].
trans_verb(person, talk_to) --> [talk,to].
trans_verb(object, look_in) --> [look,in].
trans_verb(object, look_at) --> [look,at].
trans_verb(person, look_at) --> [look,at].
trans_verb(location, go) --> [go].
trans_verb(location, go) --> [enter].
trans_verb(location, go) --> [walk].

intrans_verb(inventory) --> [inventory].
intrans_verb(look) --> [look].
intrans_verb(quit) --> [exit].
intrans_verb(quit) --> [quit].
intrans_verb(quit) --> [bye].
