% Adventure HTTP Server
% (C) 2010 Adrian Prantl

% Quit on compile-time error
user:message_hook(_Term, error, Lines) :- 
  %member(WE, [warning,error]),
  print_message_lines(user_error, 'ERROR: ', Lines),
  halt(1).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_session)).
%:- use_module(library(http/json)).
%:- use_module(library(http/json_convert)).
%:- use_module(library(http/http_json)).
:- use_module(library(time)).
:- use_module(nsols).

:- use_module(advcore2).
write_xy(Text, _, _) :- write(Text).
bold.% :- write(bold).
italic.% :- write(italic).
roman.% :- write(roman).
%getch(Ch) :- get_code(Ch).

% This has been my weekend project since early 2007. Literature:
% Montfort, Nick ... AmZi Prolog ... Galakmit Dispenser .. Knuth port

:- http_handler(root(.), welcome, []).
:- http_handler(root(run), main_loop, []).
:- http_handler(root(autocomplete), autocomplete, []).
:- http_handler(css('adventure.css'), http_reply_file('adventure.css', []), []).
:- http_handler(js_script('builder.js'), http_reply_file('contrib/builder.js', []), []).
:- http_handler(js_script('controls.js'), http_reply_file('contrib/controls.js', []), []).
:- http_handler(js_script('dragdrop.js'), http_reply_file('contrib/dragdrop.js', []), []).
:- http_handler(js_script('effects.js'), http_reply_file('contrib/effects.js', []), []).
:- http_handler(js_script('prototype.js'), http_reply_file('contrib/prototype.js', []), []).
:- http_handler(js_script('scriptaculous.js'), http_reply_file('contrib/scriptaculous.js', []), []).
:- http_handler(js_script('slider.js'), http_reply_file('contrib/slider.js', []), []).

http:location(css, root(css), []).
http:location(js_script, root(js_script), []).


server(Port) :-
  http_server(http_dispatch, [port(Port)]).

% ----------------------------------------------------------------------
% Welcome page
% ----------------------------------------------------------------------

welcome(_Request) :-
  Title = 'New! Adventure',
  History = 'Welcome!',

  % Clear state
  http_session_assert(title(_)),
  http_session_assert(history(_)),
  http_session_assert(state(_)),
  http_session_retractall(title(_)),
  http_session_retractall(history(_)),
  http_session_retractall(state(_)),

  % Launch the game
  new_game(State),
  http_session_assert(title(Title)),
  http_session_assert(history(History)),
  http_session_assert(state(State)),

  % Reply!
%  http_redirect(moved_temporary, root(run), Request).
  reply_html_page([title(Title),
		   \html_requires(css('adventure.css'))
		  ],
		  [ h1(Title),
		    p(form('action="run" method="post"',
			   [
			    input('type="hidden" name="line" value="look"'),
			    input('type="submit" value="Start"')]))
		  ]).

% ----------------------------------------------------------------------
% Main Game Loop
% ----------------------------------------------------------------------

line_sentence(Line, Sentence) :-
  atom_chars(Line, Chars),
  tokenize(Chars, ' ', Sentence).

tokenize(Chars, Seperator, Tokens) :-
  tokenize(Chars, Seperator, [], Tokens).
tokenize([Seperator|Cs], Seperator, RCs, [Token|Ts]) :- !,
  reverse(RCs, T),
  atom_chars(Token, T),
  tokenize(Cs, Seperator, [], Ts).
tokenize([], _, RCs, [Token]) :-
  reverse(RCs, T),
  atom_chars(Token, T).
tokenize([C|Cs], Seperator, RCs, Tokens) :-
  tokenize(Cs, Seperator, [C|RCs], Tokens).

main_loop(Request) :-
  http_in_session(SessionId),

  % Readline
  http_parameters(Request, [ line(Line, [default('look')]) ]),
  http_session_assert(history(['> ', span('class="reply"',Line)])),

  % Run the engine
  http_current_session(SessionId, state(State)),
  line_sentence(Line, Sentence),
  (   phrase(sentence(Action,State), Sentence)
  ->  with_output_to(atom(Reply), action(State, State1, Action)),
      http_session_assert(history(Reply)),
      http_session_retractall(state(_)),
      http_session_assert(state(State1))
  ;   http_session_assert(history('Sorry, I could not understand that!'))
  ),
  
  % Reply!
  http_current_session(SessionId, title(Title)), 
  findall(p(H), http_current_session(SessionId, history(H)), History), 

  append([[  form('action="/" method="link"',
		  [input('type="submit" value="restart"')]),
	     h1(Title)
	  ],
	  History,
	  [
           p(form('action="run" method="post"',
		  [
		   input('type="text" id="lineinput" name="line"'),
		   div('id="autocomplete_choices" class="autocomplete"',[]),
		   input('type="submit" value="Do"')
		  ])),
	   script('type=text/javascript',
		  'new Ajax.Autocompleter("lineinput",
		                          "autocomplete_choices",
		                          "/autocomplete",
		                          { method: \'get\' });')
	  ]
	 ],
	 Body),
  reply_html_page([title(Title),
		   \html_requires(css('adventure.css')),
		   \html_requires(js_script('prototype.js')),
		   \html_requires(js_script('scriptaculous.js'))
		  ], Body).


% ----------------------------------------------------------------------
% Autocompletion
% ----------------------------------------------------------------------

line_words_cs(Line, Words, Cs) :-
  atom_chars(Line, Chars),
  split1(Chars, [], Words, Cs).

% Split Chars at ' ', return list of words + list of Codes for the last Word
split1([' '|Xs], XsR, [Word|Ws], Cs) :- !,
  reverse(XsR, T),
  atom_chars(Word, T),
  split1(Xs, [], Ws, Cs).
split1([], XsR, [], Xs) :-
  reverse(XsR, Xs).
split1([X|Xs], XsR, Words, Cs) :-
  split1(Xs, [X|XsR], Words, Cs).


autocomplete(Request) :-
  http_parameters(Request, [ line(Line, [default('')]) ]),
  http_in_session(SessionId),
  http_current_session(SessionId, state(State)),

  catch(call_with_time_limit(1, findnsols(5,
					  li(C),
					  autocomplete1(State, Line, C),
					  Completions,
					  [])),
	time_limit_exceeded,
	Completions = li('<timeout>')
       ),

  % Return the autocompletion as an unsorted list
  reply_html_page([],ul(Completions)).

autocomplete1(State, Line, Completion) :- 
  line_words_cs(Line, Words, Cs),
  %trace,Words=[look,at], Cs=[],
  
  % Run the autocompletion
  atom_codes(W1, Cs),
  % Append Letters
  word(State,W2),
  atom_concat(W1, _Suffix, W2),
  % ... and words
  append([Words, [W2], _C], CsX),
  
  % and find an autocompletion
  phrase(sentence(_,State), CsX),
  %format(atom(A), '~w~n', CsX),
  atomic_list_concat(CsX, ' ', Completion).

:- guitracer.
:- server(5000).
