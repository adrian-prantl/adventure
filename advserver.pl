% -*- coding: utf-8 -*-
% Adventure HTTP Server
% (C) 2010-2011 Adrian Prantl

% Quit on compile-time error
user:message_hook(_Term, error, Lines) :- 
  %member(WE, [warning,error]),
  print_message_lines(user_error, 'ERROR: ', Lines),
  halt(1).

:- use_module(library(apply_macros)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_session)).
%:- use_module(library(http/json)).
%:- use_module(library(http/json_convert)).
%:- use_module(library(http/http_json)).
:- use_module(library(http/http_log)).
:- use_module(library(clpfd)).
:- use_module(library(time)).
:- use_module(nsols).

:- use_module(advcore2).
write_xy(Text, _, _) :- write(Text).
bold.% :- write(bold).
italic.% :- write(italic).
roman.% :- write(roman).
%getch(Ch) :- get_code(Ch).

% This has been my weekend project since early 2007. Literature:
% Montfort, Nick ... AmZi Prolog ... Galakmit Dispenser .. Knuth port .. Get Lamp

:- http_handler(root(.), welcome, []).
:- http_handler(root(run), main_loop, []).
:- http_handler(root(init), init, []).
:- http_handler(root(autocomplete), autocomplete, []).
:- http_handler(css('adventure.css'), http_reply_file('adventure.css', []), []).
:- http_handler(js_script('builder.js'), http_reply_file('contrib/builder.js', []), []).
:- http_handler(js_script('controls.js'), http_reply_file('contrib/controls.js', []), []).
:- http_handler(js_script('dragdrop.js'), http_reply_file('contrib/dragdrop.js', []), []).
:- http_handler(js_script('effects.js'), http_reply_file('contrib/effects.js', []), []).
:- http_handler(js_script('prototype.js'), http_reply_file('contrib/prototype.js', []), []).
:- http_handler(js_script('scriptaculous.js'), http_reply_file('contrib/scriptaculous.js', []), []).
:- http_handler(js_script('slider.js'), http_reply_file('contrib/slider.js', []), []).

http:location(css, root('css'), []).
http:location(js_script, root('js_script'), []).


server(Port) :-
  http_server(http_dispatch, [port(Port)]).

% ----------------------------------------------------------------------
% Welcome page
% ----------------------------------------------------------------------

welcome(_) :-
  Title = '¡New! Adventure',
  % Reply!
%  http_redirect(moved_temporary, root(run), Request).
  reply_html_page([title(Title),
		   %\html_requires(css('adventure.css'))
		   \html_requires('/adrian/adventure/css/adventure.css')
		  ],
		  [ h1(Title),
		    h2('About'),
		    p(['What you are looking at is a web interface to a text adventure engine I am developing. This has been my weekend project since one saturday afternoon in February 2007, when I decided to re-learn Prolog. The engine adds a feature I was missing most in the text adventure games I played in my teenager years: ', em(autocompletion), ' and ', em(synonyms), '.']),
		    p(['I remember it being most frustrating to know what you want to achieve, but having no idea whatsoever what a particular item or action was called by the developer of the game. My engine tries to minimize these problems.']),
		    p(small('Copyright (C) Adrian Prantl 2007–2011.')),
		     h4('References'),
		     p('These are some of the sources that a drew my inspirations from:'),
		     small([
		     p([a('href="http://nickm.com/twisty/"',
			   'Twisty Little Passages: An Approach to Interactive Fiction'),
			', Nick Montfort, The MIT Press, 2003.']),
		    p([a('href="http://www.getlamp.com/"',
			   'Get Lamp: a documentary about adventures in text'),
			', Jason Scott, DVD, 2010.']),
		    p([a('href="http://www-cs-faculty.stanford.edu/~uno/programs/advent.w.gz"',
		      'ADVENT'),
			', Donald R. Woods and Donald E. Knuth, Literate Program, Stanford University, 1998.']),
		    p([a('href="http://dx.doi.org/10.1023/B:JLLI.0000024734.80591.30"',
			   'Put My Galakmid Coin into the Dispenser and Kick It: Computational Linguistics and Theorem Proving in a Computer Game'),
			', Alexander Koller, Ralph Debusmann, Malte Gabsdil und Kristina Striegnitz, Journal of Logic, Language and Information, Volume 13, Number 2, Kluwer 2004.']),
		    p([a('href="http://www.amzi.com/AdventureInProlog/"',
			   'Adventure in Prolog'),
			', Amzi! inc., 1990.']),
		    p([a('href="http://lisperati.com/casting.html"',
			   'Casting SPELs in LISP: a comic book'),
			', Conrad Barski, Lisperati, 2006?.']),
		    p([a('href="http://wordnet.princeton.edu/"',
			   'WordNet: An Electronic Lexical Database'),
			', Princeton University, 2007.']),
                    h2('The Demo Room'),
					     
		    p(form('action="adventure/init" method="post"',
			   [
			    select('name="game"',[option('value=testgame', 'Test game'),
						                option('value=finaldays', 'Game B')]),
			    input('type="submit" value="Start the demo by clicking on this button!"')]))])
		  ]).

% ----------------------------------------------------------------------
% Main Game Loop
% ----------------------------------------------------------------------

% this will be called from main_loop
init(Request) :-
  %http_open_session(_SessionID, [renew(true)]),
  http_in_session(_SessionId),
  
  % Clear state
  http_session_assert(title(_)),
  http_session_assert(history(_)),
  http_session_assert(state(_)),
  http_session_retractall(title(_)),
  http_session_retractall(history(_)),
  http_session_retractall(state(_)),

  % Load the game definition
  http_parameters(Request, [ game(FileName, [default('testgame')]) ]), !,
  (  open(FileName, read, File, []),
     read_term(File, (Title:Game), [syntax_errors(fail)]),
     close(File)
  -> % Launch the game
     (   new_game(Game, State)
     -> http_session_assert(title(Title)),
	 http_session_assert(history('Welcome!')),
	 http_session_assert(state(State)),
	 reply_html_page([title(Title),
			  \html_requires('/adrian/adventure/css/adventure.css')
			 ],
			 [p(form('action="run" method="post" id=go',
				 [
				  input('type="hidden" name="line" value="look"'),
				  input('type="submit" value="Start!"')])),
			  script('type=text/javascript', 'document.getElementById(\'go\').submit()')])
     ; reply_html_page([title('Error'),
			\html_requires('/adrian/adventure/css/adventure.css')
		       ],
		       [p(['I\'m sorry, I could load the game ', FileName,
			   ', but the definition does not make any sense'])])
     )
     ; reply_html_page([title('Error'),
		     \html_requires('/adrian/adventure/css/adventure.css')
		    ],
		    [p(['I\'m sorry, but I could not load the game ', FileName])])
  ).

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

% Linkify some specially marked-up tokens
linkified(Atom, List) :-
  atom_chars(Atom, Chars),
  linkified1(Chars, List) .
linkified1(['~'|Chars], [Link|List]) :-
  append([[l, o, c, a, t, i, o, n,'('], Loc, [')'], Rest], Chars), !,
  atom_chars(Atom, Loc),
  format(atom(Href), 'href="run?go to the ~w")', [Atom]),
  Link = a(Href, Atom),
  linkified1(Rest, List).
linkified1(['~'|Chars], [Link|List]) :-
  append([[o,b,j,e,c,t,'('], Obj, [')'], Rest], Chars), !,
  atom_chars(Atom, Obj),
  format(atom(Href), 'href="run?line=look at the ~w"', [Atom]),
  Link = a(Href, Atom),
  linkified1(Rest, List).
linkified1(Chars, [Atom|List]) :-
  append([Cs, ['~'], Rest], Chars), !,
  atom_chars(Atom, Cs),
  linkified1(['~'|Rest], List).
linkified1(Chars, [Atom]) :-
  atom_chars(Atom, Chars).

main_loop(Request) :-
  http_in_session(SessionId),
  
  % Readline
  http_parameters(Request, [ line(Line, [default('look')]) ]),
  http_session_assert(history(['> ', span('class="command"',Line)])),

  % Run the engine
  http_current_session(SessionId, state(State)),
  line_sentence(Line, Sentence),
  (   phrase(sentence(Action, State), Sentence)
  ->  with_output_to(atom(Reply), action(State, State1, Action)),
      linkified(Reply, ReplyL),
      http_session_assert(history(ReplyL)),
      http_session_retractall(state(_)),
      http_session_assert(state(State1))
  ;   http_session_assert(history('Sorry, I could not understand that!')),
      Action=[none]
  ),
  
  % Reply!
  http_current_session(SessionId, title(Title)), 
  findall(p('class="reply"', H), http_current_session(SessionId, history(H)), History),

  Restart = form('action="/adrian/adventure" method="link"',
		 [input('type="submit" value="restart"')]),
  
  (Action = [quit|_]
  -> append([[h1(Title)],History, [Restart]], Body)
  ;  append([[Restart, h1(Title)],
	  History,
	  [
           p(form('action="run" method="post" name=userinput',
		  [
		   input('type="text" id="lineinput" name="line"'),
		   div('id="autocomplete_choices" class="autocomplete"',[]),
		   input('type="submit" value="Do"')
		  ])),
	   % additional space for the autocompletion box
	   br(''),br(''),br(''),br(''),
	   br(''),br(''),br(''),br(''),br('id=bottom'),
	   % Autocompletion, focus on input field, scroll to bottom
   	   script('type=text/javascript',
'		   new Ajax.Autocompleter("lineinput",
		                          "autocomplete_choices",
		                          "/adrian/adventure/autocomplete",
		                          { method: \'get\' });
		   lineinput.focus();
		   bottom.scrollIntoView(); ')
	  ]
	 ],
	 Body)
  ),
  reply_html_page([title(Title),
		   \html_requires('/adrian/adventure/css/adventure.css'),
		   \html_requires('/adrian/adventure/js_script/prototype.js'),
		   \html_requires('/adrian/adventure/js_script/scriptaculous.js')
		  ], Body),

  % End Session if user quits
  (Action = [quit|_]
  -> http_close_session(SessionId)
  ; true).


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
%  (Line='look a' -> gtrace ; true),
  catch(call_with_time_limit(2, findnsols(5,
					  li(C),
					  autocomplete1(State, Line, C),
					  Completions,
					  [])),
	time_limit_exceeded,
	Completions = li('<timeout>')
       ),
  % Return the autocompletion as an unsorted list
  (  Completions = []
  -> reply_html_page([],ul('I probably won\'t get that. So sorry!'))
  ;  reply_html_page([],ul(Completions))
  ).

autocomplete1(State, Line, Completion) :- 
  line_words_cs(Line, Words, Cs),
  %trace,Words=[look,at], Cs=[],
  
  % Run the autocompletion
  atom_codes(WordPrefix, Cs),

  % I think we can improve performance here by using strings instead
  % of atoms for words. This way we can provide word/2 with a prefix
  % and don't need to guess all possible words

  % Append letters
  word(State, Word),
  %member(Word, Ws),
  atom_concat(WordPrefix, _Suffix, Word),

  % ... and words
  %L in 0..2, % do only the first four words of a sentence (? isn't is sentences up to length 4?)
  (L=3;L=2;L=1;L=0),
  length(Rest, L),
  append(Words, [Word|Rest], CsX),
  print_message_lines(user_error, CsX,[]),
  
  % and find an autocompletion
  phrase(sentence(_,State), CsX),
  %format(atom(A), '~w~n', CsX),
  atomic_list_concat(CsX, ' ', Completion).

:- guitracer.
:- server(8002).
