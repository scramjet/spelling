PROF_FLAGS = -prof -auto-all

# FLAGS = -O2 -fforce-recomp -fexcess-precision -funbox-strict-fields \
# 	-fglasgow-exts -fvia-c -optc-O3 -optc-ffast-math -Wall \
# 	-funfolding-keeness-factor=10

FLAGS = -O2 -fglasgow-exts -Wall

spelling: spelling.hs
	ghc --make $(FLAGS) spelling.hs
