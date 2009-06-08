% Adventure core
% (C) 2007-2009 Adrian Prantl
:- module(advcore2, [main/0]).

main:-
  cformat('Welcome!~n', []),
  cwrite('>'),
  io_loop([],[]).

cwrite(T) :- write_xy(T, -1, -1).
cformat(Format, Arguments) :-
  format(atom(T), Format, Arguments),
  cwrite(T).
format_xy(Format, Arguments, X, Y) :-
  format(atom(T), Format, Arguments),
  write_xy(T, X, Y).

io_loop(WordsR, CsR) :-
  format_xy('Words = ~w, Cs = ~w~n', [WordsR,CsR], 0, 22),
  getch(Char), !,
  format_xy('Char = ~c (~w)~n', [Char,Char], 24, 0),
  handle_char(Char, WordsR, CsR).

% Switch over curren Char
handle_char(end_of_file, _, _) :- !.
handle_char(C, WsR, CsR) :- char_code(' ', C),  !, handle_whitespace(WsR, CsR).
handle_char(C, WsR, CsR) :-      backspace(C),  !, handle_backspace(WsR, CsR).
handle_char(C, WsR, CsR) :- char_code('\t', C), !, autocomplete(WsR, CsR).
handle_char(C, WsR, CsR) :- io_loop(WsR, [C|CsR]).

%-----------------------------------------------
% Ignore Whitespace
handle_whitespace(WordsR, []) :- !,
  cwrite('\b'),
  io_loop(WordsR, []).
handle_whitespace(WordsR, CsR) :-
  reverse(CsR, Cs),
  atom_codes(W, Cs),
  io_loop([W|WordsR], []).

%-----------------------------------------------
% Handle Backspace
backspace(127) :- cwrite('\b').
backspace(8).

handle_backspace([], []) :- !,
  io_loop([], []).
handle_backspace([W|WordsR], []) :- !,
  atom_codes(W, Cs),
  reverse(Cs, [_|CsR]),
  io_loop(WordsR, CsR).
handle_backspace(WordsR, [_|CsR]) :-
  io_loop(WordsR, CsR).

%-----------------------------------------------
% Autocompletion
letters(Ls) :-
  length(Ls, _N),
  maplist(letter, Ls).

letter(L) :- char_type(L, alpha).

finishes_sentence(WordsR, CsR, Letters, Words) :-
  reverse(Cs, CsR),
  atom_codes(W1, Cs),
  % Append Letters
  letters(Letters),
  concat_atom([W1|Letters], W2),
  % ... and words
  reverse(Ws, [W2|WordsR]),
  append(Ws, Words, CsX),
  % and find an autocompletion
  phrase(sentence(_), CsX).


autocomplete(WordsR, CsR) :-
  cwrite('\b\b\b\b\b['),
  recorda(count, 0),
  repeat,

  finishes_sentence(WordsR, CsR, AC_Letters, AC_Words),
  % Console output
  concat_atom(AC_Letters, A), cwrite(A),
  maplist(cwrite_, AC_Words),
  cwrite('\n'),

  recorded(count, N), N1 is N+1,
  recorda(count, N1),
  (N > 3 -> true; fail),

  cwrite(']\n'),
  print_line(WordsR, CsR),
  io_loop(WordsR, CsR).
autocomplete(_, _) :- cwrite('ERROR').

cwrite_(X) :- cwrite(' '), cwrite(X).

print_line(WordsR, CsR) :-
  reverse(Cs, CsR),
  atom_codes(W, Cs),

  reverse(Words, WordsR),
  maplist(cwrite_, Words),
  cwrite(W).

%-----------------------------------------------
% GRAMMAR
sentence([Verb, Noun]) --> verb(Type, Verb), nounphrase(Type, Noun).

det --> [the].
det --> [a].
det --> [an].

nounphrase(Type,Noun) --> det,noun(Type,Noun).
nounphrase(Type,Noun) --> noun(Type,Noun).

noun(thing,light) --> [light].

verb(thing,V) --> tran_verb(V).
verb(intran,V) --> intran_verb(V).


tran_verb(take) --> [take].
tran_verb(drop) --> [drop].
tran_verb(eat) --> [eat].
tran_verb(turn_on) --> [turn,on].
tran_verb(turn_off) --> [turn,off].
tran_verb(talk_to) --> [talk,to].
tran_verb(look_in) --> [look,in].
tran_verb(look_at) --> [look,at].

intran_verb(inventory) --> [inventory].
intran_verb(look) --> [look].
intran_verb(quit) --> [exit].
