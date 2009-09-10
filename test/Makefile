GHC = ghc
ARROWP = ccap
FLAGS = -fvia-C -fno-method-sharing -fexcess-precision -O2 -XTemplateHaskell 

Main: Main.hs Sample.hs
	$(GHC) $(FLAGS) --make Main.hs

%.hs: %.as
	$(ARROWP) $< > $@

clean: 
	rm -f Main *.o *.hi Sample.hs

