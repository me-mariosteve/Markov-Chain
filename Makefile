Markov.o: Markov.hs
	ghc -dynamic -c -o Markov.o Markov.hs

ghci:
	ghci -dynamic Markov.hs

clean:
	rm -f *.o *.hi

