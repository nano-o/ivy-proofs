.PHONY: all clean test

CXX = g++
CXXFLAGS = -std=c++11 `pkg-config --cflags xdrpp` -I.
LIBS = `pkg-config --libs xdrpp`
XDRC = xdrc

SRC_X_FILES = $(wildcard xdr/*.x)
SRC_HPP_FILES = $(wildcard libscp/*.hpp vendor/*.hpp)
SRC_EXE = $(wildcard app/*.cpp)

%.h: %.x
	$(XDRC) -hh -pedantic -o $@ $<

all: $(SRC_X_FILES:.x=.h) $(SRC_EXE:app/%.cpp=%.out)

clean:
	rm -vf $(SRC_X_FILES:.x=.h) $(SRC_EXE:app/%.cpp=%.out) $(SRC_HPP_FILES:libscp/%.hpp=%.o)
	rm -vfr $(SRC_EXE:app/%.cpp=%.out.dSYM/)
	rm -vf data/*.xdr

test: all
	./GenTestData.out < data/conflicted.
	./TestCompilation.out

# build executables in ./app/ by giving their names suffixed by .out
%.out: app/%.cpp $(SRC_X_FILES:.x=.h) $(SRC_HPP_FILES)
	$(CXX) $(CXXFLAGS) -g -o $@ $(filter-out %.h %.hpp,$^) $(LIBS)

# build objects in ./libscp/ by giving their names suffixed by .o
%.o: libscp/%.hpp $(SRC_X_FILES:.x=.h)
	$(CXX) $(CXXFLAGS) -o $@ -c $(filter-out %.h,$^) $(LIBS)

# watch any target by prefixing its name with "watch-"
watch-%:
	export PKG_CONFIG_PATH="$$PKG_CONFIG_PATH:$$HOME/.local/lib/pkgconfig"; \
	export PATH="$$PATH:$$HOME/.local/bin"; \
	git ls-files | entr -c bash -c 'make $*'
