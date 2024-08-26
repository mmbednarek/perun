.PHONY: all clean

snake.o: snake.pr
	./target/debug/perun snake.pr -o snake.o

snake: snake.o
	gcc snake.o -lSDL2 -o snake

all: snake

clean:
	rm snake.o
	rm snake