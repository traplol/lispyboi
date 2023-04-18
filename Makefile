CC := clang++-15
CFLAGS := -std=c++17 -Wall -Wno-unused-function
LDFLAGS := -ldl -fPIE 

SOURCES:= $(wildcard src/*/*.cpp)
SOURCES+= $(wildcard src/*.cpp)
OBJECTS:= $(addprefix obj/,$(notdir $(SOURCES:.cpp=.o)))

.PHONY: all clean _debug debug debug3 debug2 debug1 release

all: debug1

debug: debug1

debug3: CFLAGS += -O0 -g3 -DDEBUG=3
debug3: CFLAGS += -fsanitize=address -fsanitize-recover=all
debug3: _debug

debug2: CFLAGS += -O0 -g3 -DDEBUG=2
debug2: _debug

debug1: CFLAGS += -O1 -g3 -DDEBUG=1
debug1: _debug

_debug: CFLAGS += -fno-omit-frame-pointer -fno-optimize-sibling-calls
_debug: lispyboi

release: CFLAGS += -O3 -DDEBUG=0 -flto
release: lispyboi

lispyboi: $(OBJECTS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

obj/%.o: src/*/%.cpp | obj
	$(CC) $(CFLAGS) -c -o $@ $<

obj/%.o: src/%.cpp | obj
	$(CC) $(CFLAGS) -o $@ -c $<

obj:
	mkdir -p $@

clean:
	rm -rf obj lispyboi
