% Adventure core
% (C) 2007-2009 Adrian Prantl
:- module(advcore2, [main/0]).

main :-
  cformat('Welcome!~n', []),
  new_command.

bye :- cformat('~nGoodbye!~n', []).

cwrite(T) :- write_xy(T, -1, -1).
cformat(Format, Arguments) :-
  format(atom(T), Format, Arguments),
  cwrite(T).
format_xy(Format, Arguments, X, Y) :-
  format(atom(T), Format, Arguments),
  write_xy(T, X, Y).

new_command :-
  prompt,
  io_loop([],[]).

% Display the prompt and optionally redraw the user input
prompt :- bold, cwrite('\r>'), roman.
prompt(WsR, CsR) :-
  prompt,
  lists_sentence(WsR, CsR, Sentence),
  maplist(cwrite_, Sentence).

io_loop(WordsR, CsR) :-
  format_xy('Words = ~w, Cs = ~w~n', [WordsR,CsR], 0, 23),
  autocomplete(WordsR, CsR),
  getch(Char), !,
  format_xy('Char = ~c (~w)~n', [Char,Char], 64, 0),
  handle_char(Char, WordsR, CsR).

% Switch over current Char
handle_char(C, WsR, CsR) :- char_code(' ', C),   handle_whitespace(WsR, CsR).
handle_char(C, WsR, CsR) :- char_code('\t', C),  handle_tabulator(WsR, CsR).
handle_char(C, WsR, CsR) :- backspace(C),        handle_backspace(WsR, CsR).
handle_char(C, WsR, CsR) :- char_type(C, alnum), io_loop(WsR, [C|CsR]).
handle_char(C,_WsR,_CsR) :- char_type(C, end_of_file), bye.
handle_char(C, WsR, CsR) :- char_type(C, end_of_line), 
  lists_sentence(WsR, CsR, Sentence),
  (phrase(sentence(A), Sentence)
  -> cwrite('\n'),
   action(A),
   new_command
  ; prompt(WsR, CsR),
   italic, cwrite('\nSorry, I could not understand that!\n'), roman,
   new_command
   %cwrite('>'),
   %io_loop(WsR, CsR)
  ).
handle_char(_, WsR, CsR) :- % ignore
  prompt(WsR, CsR),
  io_loop(WsR, CsR).

lists_sentence(WordsR, CharsR, Sentence) :-
  reverse(CharsR, Cs),
  atom_codes(W, Cs),
  reverse(Sentence, [W|WordsR]).

%-----------------------------------------------
handle_whitespace(WordsR, []) :- !,
  cwrite('\b'), % Ignore Whitespace
  io_loop(WordsR, []).
handle_whitespace(WordsR, CsR) :-
  reverse(CsR, Cs), % finish this word
  atom_codes(W, Cs),
  io_loop([W|WordsR], []).
handle_tabulator(WordsR, CsR) :-
  cwrite('\b\b\b\b'), % ignore tab
  io_loop(WordsR, CsR).

%-----------------------------------------------
% Handle Backspace
backspace(127) :- cwrite('\b\b\b   \b\b\b').
backspace(8).

handle_backspace([], []) :- !,
  io_loop([], []).
handle_backspace([W|WordsR], []) :- !,
  cwrite('\b \b'), % get rid of that extra space
  atom_codes(W, Cs),
  reverse(Cs, [_|CsR]),
  io_loop(WordsR, CsR).
handle_backspace(WordsR, [_|CsR]) :-
  io_loop(WordsR, CsR).

%-----------------------------------------------
% Autocompletion

finishes_sentence(WordsR, CsR, Suffix, Words) :-
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
finishes_sentence(_, _, ' ', []) :- repeat.

autocomplete(WordsR, CsR) :-
  %cwrite('\b\b\b\b\b['),
  recorda(count, 0),

  finishes_sentence(WordsR, CsR, Suffix, AC_Words),
    % Console output
    once(recorded(count, N)),
    %trace, writeln(N),
    Line is 1 + N,
    append([[Suffix], AC_Words, ['.','','','']], S),
    format_xy('~w ~w ~w ~w ~w~n', S, 40, Line),
    N1 is N+1, recorda(count, N1),
  (N >= 3 -> true; fail).

  %cwrite(']\n'),
  %print_line(WordsR, CsR),
  %io_loop(WordsR, CsR).
autocomplete(_, _) :- cwrite('ERROR').

cwrite_(X) :- cwrite(' '), cwrite(X).

print_line(WordsR, CsR) :-
  reverse(Cs, CsR),
  atom_codes(W, Cs),

  reverse(Words, WordsR),
  maplist(cwrite_, Words),
  cwrite(W).


action(S) :-
  Goal =.. S,
  catch(Goal, E, error(Goal, E)).
error(Goal, E) :-
  writeln(E),
  Goal =.. [F|_],
  italic, 
  cformat('Sorry, I don\'t know how to ~w properly.~n', [F]),
  roman.

%-----------------------------------------------
% BASIC GAME MECHANICS

noun_type(object).
noun_type(location).
noun_type(person).

% nouns
object(light).
object(bread).
object(door).
object(lighter).

location([living,room]).
location(kitchen).

person(gnome).

% properties
inflammable(wood).
bibulous(wood).

material(door, wood).

% actions
%go(Location) :-
%  recorda...

%-----------------------------------------------
% GRAMMAR

% Begin - reverse rules
word(W) :- phrase(det, Ws),       member(W, Ws).
word(W) :- phrase(verb(_,_), Ws), member(W, Ws).
word(W) :- phrase(adverb(_), Ws), member(W, Ws).
word(W) :- noun_type(T), phrase(noun(T,_), Ws),        member(W, Ws).
word(W) :- noun_type(T), phrase(preposition(T,_), Ws), member(W, Ws).
% End - reverse rules

sentence([Verb, Noun]) --> verb(Type, Verb), nounphrase(Type, Noun).
sentence([Verb, Noun]) -->
  verb(Type, Verb), preposition(Type,_), nounphrase(Type, Noun).

det --> [the].
det --> [my].
%det --> [a].
%det --> [an].

nounphrase(Type,Noun) --> det,noun(Type,Noun).
nounphrase(Type,Noun) --> noun(Type,Noun).

noun(Type, Noun) --> { call(Type,Noun), atom(Noun) }, [Noun].
noun(Type, Noun) --> { call(Type,Noun), is_list(Noun) }, Noun.

verb(Type,V) --> trans_verb(Type,V).
verb(intrans,V) --> intrans_verb(V).

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
trans_verb(_, look_at) --> [look,at].
trans_verb(location, go) --> [go].
trans_verb(location, go) --> [enter].
trans_verb(location, go) --> [walk].

intrans_verb(inventory) --> [inventory].
intrans_verb(look) --> [look].
intrans_verb(quit) --> [exit].
