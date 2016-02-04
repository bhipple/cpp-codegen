.PHONY:
all: gentest

gentest: gentest.hs
	ghc --make -o gentest gentest.hs
