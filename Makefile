.PHONY: all clean

corelib.o: corelib.pr
	./target/debug/perun corelib.pr -o corelib.o

snake.o: snake.pr
	./target/debug/perun snake.pr -o snake.o

snake: snake.o corelib.o
	gcc snake.o corelib.o -lSDL2 -o snake

all: snake

clean:
	rm corelib.o
	rm snake.o
	rm snake