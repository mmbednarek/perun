.PHONY: all clean

BUILD_DIR=build-dir
PERUNC="./../target/debug/perun"

all: $(BUILD_DIR)/snake $(BUILD_DIR)/linked_list

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

$(BUILD_DIR)/corelib.o: $(BUILD_DIR) corelib.pr
	$(PERUNC) corelib.pr -o $(BUILD_DIR)/corelib.o

$(BUILD_DIR)/snake.o: $(BUILD_DIR) snake.pr
	$(PERUNC) snake.pr -o $(BUILD_DIR)/snake.o

$(BUILD_DIR)/linked_list.o: $(BUILD_DIR) linked_list.pr
	$(PERUNC) linked_list.pr -o $(BUILD_DIR)/linked_list.o

$(BUILD_DIR)/snake: $(BUILD_DIR)/snake.o $(BUILD_DIR)/corelib.o
	gcc $(BUILD_DIR)/snake.o $(BUILD_DIR)/corelib.o -lSDL2 -o $(BUILD_DIR)/snake

$(BUILD_DIR)/linked_list: $(BUILD_DIR)/linked_list.o $(BUILD_DIR)/corelib.o
	gcc $(BUILD_DIR)/linked_list.o $(BUILD_DIR)/corelib.o -o $(BUILD_DIR)/linked_list

clean:
	rm -rf build-dir