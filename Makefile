Markov.o: Markov.hs
	ghc -dynamic $(GHCFLAGS) -c -o Markov.o Markov.hs

ghci:
	ghci -dynamic $(GHCIFLAGS) Markov.hs

clean:
	rm -f *.o *.hi

