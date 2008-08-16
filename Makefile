BASE=$(shell swipl --dump-runtime-variables | sed 's/"/ /g' | awk '/PLBASE/ {print $$2}')
PLFLAGS=-L$(shell dirname `find $(BASE) -name libpl.a`) -lpl


embed: embed.c
	gcc $(PLFLAGS) -o embed embed.c
