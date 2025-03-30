CXX = g++
CXXFLAGS = -std=c++11 `pkg-config --cflags xdrpp` -I.
LIBS = `pkg-config --libs xdrpp`
SRC_X_FILES = xdr/Stellar-SCP.x xdr/Stellar-types.x
XDRC = xdrc

%.h: %.x
	$(XDRC) -hh -pedantic -o $@ $<

all: $(SRC_X_FILES:.x=.h) test

clean:
	rm -f $(SRC_X_FILES:.x=.h) test

test: TestQuorumSet.cpp $(SRC_X_FILES:.x=.h)
	$(CXX) $(CXXFLAGS) -o test TestQuorumSet.cpp $(LIBS)
