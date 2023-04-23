CC := clang++
CFLAGS := -std=c++17 -Wall -Wno-unused-function
LDFLAGS := -ldl -fPIE 

SOURCES:= $(wildcard src/*/*.cpp)
SOURCES+= $(wildcard src/*.cpp)
OBJECTS:= $(addprefix obj/,$(notdir $(SOURCES:.cpp=.o)))

.PHONY: all clean _debug debug debug3 debug2 debug1 release

#all: debug1
all: tailcalls_debug

debug: debug1

debug3: CFLAGS += -O0 -g3 -DDEBUG=3
#debug3: CFLAGS += -fsanitize=address -fsanitize-recover=all
debug3: _debug

debug2: CFLAGS += -O0 -g3 -DDEBUG=2
debug2: _debug

debug1: CFLAGS += -O1 -g3 -DDEBUG=1
debug1: _debug

_debug: CFLAGS += -foptimize-sibling-calls
_debug: lispyboi

release: CFLAGS += -O3 -DDEBUG=0 -flto
release: lispyboi

tailcalls_debug: CFLAGS += -O1 -DDEBUG=1
tailcalls_debug: CFLAGS += -DUSE_TAILCALLS=1 -DUSE_COMPUTED_GOTOS=0
#tailcalls_debug: CFLAGS += -g -fprofile-instr-generate -fcoverage-mapping
#tailcalls_debug: LDFLAGS += -fprofile-instr-generate
tailcalls_debug: _debug

tailcalls: CFLAGS += -O3 -g3 -DDEBUG=0 -DUSE_TAILCALLS=1 -DUSE_COMPUTED_GOTOS=0 -flto
tailcalls: lispyboi

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
