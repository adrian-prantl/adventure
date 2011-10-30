BASE=$(shell swipl --dump-runtime-variables | sed 's/"/ /g' | awk '/PLBASE/ {print $$2}')
PLFLAGS=-L$(shell dirname `find $(BASE) -name libpl.a`) -lpl

all: embed

clean:
	rm -f embed

debug:
	swipl -f debug.pl

run: embed
	./embed

www:
	open http://localhost:8002 && swipl -f advserver.pl

embed: embed.c
	gcc $(PLFLAGS) -I$(BASE)/include -o embed embed.c -lcurses
