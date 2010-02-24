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
:- use_module(library(http/http_session)).

:- use_module(advcore2).
write_xy(Text, _, _) :- write(Text).
bold.% :- write(bold).
italic.% :- write(italic).
roman.% :- write(roman).
%getch(Ch) :- get_code(Ch).


:- http_handler(root(.), welcome, []).
:- http_handler(root(q), main_loop, []).

server(Port) :-
        http_server(http_dispatch, [port(Port)]).

welcome(_) :-
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
  reply_html_page(title(Title),
		  [ h1(Title),
		    p(History),
		    p(form('action="q" method="get"',
			   [
			    input('type="text" name="line"'),
			    input('type="submit"')]))
		  ]).

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
  http_parameters(Request, [ line(Line, [default='']) ]),
  http_session_assert(history(['> ', span('style="reply"',Line)])),

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
           p(form('action="q" method="get"',
		  [
		   input('type="text" name="line"'),
		   input('type="submit"')
		  ]))
	  ]
	 ],
	 Body),
  reply_html_page(title(Title), Body).

:- server(5000).
