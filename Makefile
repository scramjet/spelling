.SUFFIXES: .hs .o

SOURCES = spelling.hs spelling_df.hs
FLAGS = -fglasgow-exts -Wall
OPT_VIA_C = -funbox-strict-fields -fvia-C -optc-O2
PROF_FLAGS = -prof -auto-all

.hs.o:
	ghc --make $(FLAGS) $(OPT_VIA_C) $<

all: compile

compile: ${SOURCES:%.hs=%.o}

clean:
	rm spelling spelling.o spelling.hi
