.PHONY: all clean watch

CPP=g++ -std=c++17 -Wall -pedantic
H_SRC=$(wildcard *.h)
CPP_SRC=$(wildcard *.cpp)
OUT=bla

all: $(H_SRC) $(CPP_SRC)
	$(CPP) $(H_SRC) $(CPP_SRC) -o $(OUT)

clean:
	rm -vf $(OUT)

watch:
	git ls-files | entr -c time make
