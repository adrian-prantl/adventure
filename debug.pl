:- use_module(advcore2).

write_xy(Text, _, _) :- write(Text).
bold.
italic.
roman.
getch(Ch) :- get_code(Ch).

:- guitracer.

:- main(finaldays).
