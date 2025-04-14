.PHONY: all clean test check

CXX = g++
CXXFLAGS = -std=c++11 `pkg-config --cflags xdrpp` -I.
LIBS = `pkg-config --libs xdrpp`
XDRC = xdrc

SRC_X_FILES = $(wildcard xdr/*.x)
SRC_HPP_FILES = $(wildcard libscp/*.hpp vendor/*.hpp)
SRC_IVY_FILES = $(wildcard *.ivy)
SRC_EXE = $(wildcard app/*.cpp)

%.h: %.x
	$(XDRC) -hh -pedantic -o $@ $<

all: $(SRC_X_FILES:.x=.h) $(SRC_EXE:app/%.cpp=%.out) paxos.out

clean:
	rm -vf $(SRC_X_FILES:.x=.h) $(SRC_EXE:app/%.cpp=%.out) $(SRC_HPP_FILES:libscp/%.hpp=%.o)
	rm -vfr $(SRC_EXE:app/%.cpp=%.out.dSYM/)
	rm -vf data/*.xdr
	rm -vf paxos.{dsc,h} app/paxos.cpp

test: all
	./GenTestData.out < data/conflicted.json
	./TestCompilation.out

check:
	ivy_check isolate=level_1 paxos.ivy
	ivy_check isolate=level_2 paxos.ivy
	ivy_check isolate=impl complete=fo paxos.ivy

# build executables in ./app/ by giving their names suffixed by .out
%.out: app/%.cpp $(SRC_X_FILES:.x=.h) $(SRC_HPP_FILES)
	$(CXX) $(CXXFLAGS) -g -o $@ $(filter-out %.h %.hpp,$^) $(LIBS)

# paxos repl app
app/paxos.cpp: $(SRC_IVY_FILES)
	ivy_to_cpp target=repl isolate=imp_extract paxos.ivy # instead use target=impl if we want to include paxos.cpp as a library?
	echo '#include <libscp/PaxosAdapter.hpp>' > $@
	cat paxos.cpp >> $@
	rm -f paxos.cpp


# watch any target by prefixing its name with "watch-"
watch-%:
	export PKG_CONFIG_PATH="$$PKG_CONFIG_PATH:$$HOME/.local/lib/pkgconfig"; \
	export PATH="$$PATH:$$HOME/.local/bin"; \
	git ls-files | entr -c bash -c 'make $*'
