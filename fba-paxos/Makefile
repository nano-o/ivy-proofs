CXX = g++
CXXFLAGS = -std=c++11 `pkg-config --cflags xdrpp` -I.
LIBS = `pkg-config --libs xdrpp`
SRC_X_FILES = xdr/Stellar-SCP.x xdr/Stellar-types.x
XDRC = xdrc

%.h: %.x
	$(XDRC) -hh -pedantic -o $@ $<

all: $(SRC_X_FILES:.x=.h) test is_quorum

clean:
	rm -f $(SRC_X_FILES:.x=.h) test is_quorum

test: TestQuorumSet.cpp $(SRC_X_FILES:.x=.h)
	$(CXX) $(CXXFLAGS) -o $@ $< $(LIBS)

is_quorum: QuorumChecker.cpp $(SRC_X_FILES:.x=.h)
	$(CXX) $(CXXFLAGS) -o $@ $< $(LIBS)

watch:
	export PKG_CONFIG_PATH="$$PKG_CONFIG_PATH:$$HOME/.local/lib/pkgconfig"; \
	export PATH="$$PATH:$$HOME/.local/bin"; \
	git ls-files | entr -c time bash -c 'make is_quorum && ./is_quorum'
