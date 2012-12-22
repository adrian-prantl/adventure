% Minimal Prolog-only debug driver. Use it to single-step through the
% adventure core.
%
% (C) 2007, 2012 Adrian Prantl
% This file is part of Adventure.
%
% Adventure is free software: you can redistribute it and/or modify
% it under the terms of the GNU Affero General Public License as
% published by the Free Software Foundation, either version 3 of the
% License, or (at your option) any later version.
%
% Adventure is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Affero General Public License for more details.
%
% You should have received a copy of the GNU Affero General Public
% License along with Adventure.  If not, see
% <http://www.gnu.org/licenses/>.
%
:- use_module(advcore2).

write_xy(Text, _, _) :- write(Text).
bold.
italic.
roman.
getch(Ch) :- get_code(Ch).

:- guitracer.

:- main(finaldays).
