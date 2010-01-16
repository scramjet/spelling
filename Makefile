PROF_FLAGS = -prof -auto-all

FLAGS = -O2 -fforce-recomp -fexcess-precision -funbox-strict-fields \
	-fvia-c -optc-O3 -optc-ffast-math -funfolding-keeness-factor=10

spelling: spelling.hs
	ghc --make $(FLAGS) $(PROF_FLAGS) spelling.hs
