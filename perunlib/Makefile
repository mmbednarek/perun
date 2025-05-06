.PHONY: all clean

BUILD_DIR=../build-dir
PERUNC="./../target/debug/perun"

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

$(BUILD_DIR)/root.o $(BUILD_DIR)/root.json: $(BUILD_DIR) root.pr
	$(PERUNC) root.pr -o $(BUILD_DIR)/root.o -m $(BUILD_DIR)/root.json --no-include-root

$(BUILD_DIR)/perunlib.a: $(BUILD_DIR)/root.o
	ar rcs $@ $^

clean:
	rm -rf $(BUILD_DIR)

all: $(BUILD_DIR)/perunlib.a