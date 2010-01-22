# FLAGS = -O2 -fforce-recomp -fexcess-precision -funbox-strict-fields \
# 	-fglasgow-exts -fvia-c -optc-O3 -optc-ffast-math -Wall \
# 	-funfolding-keeness-factor=10

FLAGS = -fglasgow-exts -Wall

OPT_VIA_C = -funbox-strict-fields -fvia-C -optc-O2

PROF_FLAGS = -prof -auto-all

spelling: spelling.hs Makefile
	ghc --make $(FLAGS) $(OPT_VIA_C) $(PROF_FLAGS) spelling.hs

clean:
	rm spelling spelling.o spelling.hi