# (C) 2007-2012 Adrian Prantl
# This file is part of Adventure.
#
# Adventure is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# Adventure is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public
# License along with Adventure.  If not, see
# <http://www.gnu.org/licenses/>.
#
BASE=$(shell swipl --dump-runtime-variables | sed 's/"/ /g' | awk '/PLBASE/ {print $$2}')
PLFLAGS=-L$(shell dirname `find $(BASE) -name libswipl.a`) -lswipl

all: embed

clean:
	rm -f embed

debug:
	swipl -f debug.pl

run: embed
	./embed

check:
	@echo "Running DejaGNU testsuite..."
	runtest testsuite.exp

www:
	open http://localhost:8002 && sh run.sh

embed: embed.c
	cc $(PLFLAGS) -I$(BASE)/include -o embed -g embed.c -lcurses
