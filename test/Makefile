GHC = ghc
ARROWP = ccap
FLAGS = -fexcess-precision -O2 -XTemplateHaskell 

Main: Main.hs Sample.hs Sample1.hs
	$(GHC) $(FLAGS) --make Main.hs

%.hs: %.as
	$(ARROWP) $< > $@

clean: 
	rm -f Main *.o *.hi Sample.hs

