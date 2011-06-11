#!swipl -q -t main -f
:- use_module('contrib/wordnet/wn_s').
:- use_module('contrib/wordnet/wn_g').

main :-
  current_prolog_flag(argv, Argv),
  append(_, [--|[Arg]], Argv),
  s(S, _, Arg, _, _, _),
  g(S, Gloss),
  nl,
  format('----------~n~w:~n~w~n', [S, Gloss]),
  s(S, _, Synonym, _, _, _),
  write(Synonym), write(', '),
  fail.
main :- nl.