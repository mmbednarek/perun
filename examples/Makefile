.PHONY: all clean

BUILD_DIR=build-dir
PERUNC="./../target/debug/perun"

all: $(BUILD_DIR)/snake $(BUILD_DIR)/linked_list

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

$(BUILD_DIR)/snake.o: $(BUILD_DIR) snake.pr
	$(PERUNC) snake.pr -o $(BUILD_DIR)/snake.o

$(BUILD_DIR)/linked_list.o: $(BUILD_DIR) linked_list.pr
	$(PERUNC) linked_list.pr -o $(BUILD_DIR)/linked_list.o

$(BUILD_DIR)/method_test.o: $(BUILD_DIR) method_test.pr
	$(PERUNC) method_test.pr -o $(BUILD_DIR)/method_test.o

$(BUILD_DIR)/casting.o: $(BUILD_DIR) casting.pr
	$(PERUNC) casting.pr -o $(BUILD_DIR)/casting.o

$(BUILD_DIR)/io.o: $(BUILD_DIR) io.pr
	$(PERUNC) io.pr -o $(BUILD_DIR)/io.o

$(BUILD_DIR)/sdl.o: $(BUILD_DIR) sdl.pr
	$(PERUNC) sdl.pr -o $(BUILD_DIR)/sdl.o

$(BUILD_DIR)/mem.o: $(BUILD_DIR) mem.pr
	$(PERUNC) mem.pr -o $(BUILD_DIR)/mem.o

$(BUILD_DIR)/random.o: $(BUILD_DIR) random.pr
	$(PERUNC) random.pr -o $(BUILD_DIR)/random.o

$(BUILD_DIR)/snake: $(BUILD_DIR)/snake.o $(BUILD_DIR)/io.o $(BUILD_DIR)/casting.o $(BUILD_DIR)/sdl.o $(BUILD_DIR)/random.o $(BUILD_DIR)/mem.o
	gcc $(BUILD_DIR)/snake.o $(BUILD_DIR)/io.o $(BUILD_DIR)/casting.o $(BUILD_DIR)/sdl.o $(BUILD_DIR)/random.o $(BUILD_DIR)/mem.o -lSDL2 -o $(BUILD_DIR)/snake

$(BUILD_DIR)/linked_list: $(BUILD_DIR)/linked_list.o $(BUILD_DIR)/corelib.o
	gcc $(BUILD_DIR)/linked_list.o $(BUILD_DIR)/corelib.o -o $(BUILD_DIR)/linked_list

$(BUILD_DIR)/method_test: $(BUILD_DIR)/method_test.o $(BUILD_DIR)/casting.o $(BUILD_DIR)/io.o
	gcc $(BUILD_DIR)/method_test.o  $(BUILD_DIR)/io.o $(BUILD_DIR)/casting.o -o $(BUILD_DIR)/method_test

clean:
	rm -rf build-dir