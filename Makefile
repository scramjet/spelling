.SUFFIXES: .hs .o

SOURCES = spelling.hs spelling_df.hs
FLAGS = -fglasgow-exts -Wall
CODEGEN_VIA_C = -O2 -funbox-strict-fields -fvia-C -optc-O2
CODEGEN_VIA_GHC = -O2 -funbox-strict-fields
PROF = -prof -auto-all

.hs.o:
	ghc --make $(FLAGS) $(CODEGEN_VIA_GHC) $<

all: compile

compile: ${SOURCES:%.hs=%.o}

clean:
	rm spelling spelling.o spelling.hi spelling_df spelling_df.o spelling_df.hi
