:- module(nsols, [findnsols/4,findnsols/5]).

:- meta_predicate
        findnsols(+, ?, :, -),
        findnsols(+, ?, :, -, ?).

%%      findnsols(+N, ?Template, :Generator, -List)
%
%       As findall/3, but generating at most   N solutions of Generator.
%       Thus, the length of List will not   be  greater than N. If N=<0,
%       returns directly an empty  list.   This  predicate is especially
%       useful if Generator may have an infinite number of solutions.
%
%       @compat ciao

findnsols(N, Template, Generator, List) :-
        findnsols(N, Template, Generator, List, []).

%%      findnsols(+N, ?Template, :Generator, -List, -Tail)
%
%       As findnsols/4, but returning in Tail the tail of List.
%
%       @compat ciao

findnsols(N, Template, Generator, List, Tail) :-
        findall(Template, maxsols(N, Generator), List, Tail).

maxsols(N, Generator) :-
        State = count(0),
        Generator,
        arg(1, State, C0),
        C1 is C0+1,
        (   C1 == N
        ->  !
        ;   nb_setarg(1, State, C1)
        ).

