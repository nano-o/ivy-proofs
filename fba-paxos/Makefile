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
	$(CXX) $(CXXFLAGS) -o $@ $(filter-out %.h,$^) $(LIBS)

watch-%:
	export PKG_CONFIG_PATH="$$PKG_CONFIG_PATH:$$HOME/.local/lib/pkgconfig"; \
	export PATH="$$PATH:$$HOME/.local/bin"; \
	git ls-files | entr -c time bash -c 'make $* && ./$*'


### xdrgen tool

GEMHOME = ./gem-home
GEMENV = env GEM_HOME=$(GEMHOME)
GEM = $(GEMENV) gem

GEM_CONCURRENT_RUBY = $(GEMHOME)/gems/concurrent-ruby-1.3.4
GEM_ZEITWERK = $(GEMHOME)/gems/zeitwerk-2.6.18
GEM_XDRGEN = $(GEMHOME)/bin/xdrgen

$(GEMHOME):
	-mkdir -p $(GEMHOME)

$(GEM_CONCURRENT_RUBY): $(GEMHOME)
	$(GEM) install -V concurrent-ruby -v 1.3.4

$(GEM_ZEITWERK): $(GEMHOME)
	$(GEM) install -V zeitwerk -v 2.6.18

$(GEM_XDRGEN): $(GEMHOME) $(GEM_CONCURRENT_RUBY) $(GEM_ZEITWERK)
	$(GEM) install -V xdrgen -v 0.1.1

xdr-rb:
	$(GEMENV) $(GEM_XDRGEN) -o xdr-rb xdr/Stellar-SCP.x
