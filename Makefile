PROG=./Factorize.exe
SRC =Factorize.hs

run: $(PROG)
	csh ./run.csh
#	$(PROG) +RTS -N -s -RTS
$(PROG): $(SRC)
	ghc -O2 $(SRC) -o $(PROG)
#	ghc -O2 $(SRC) -threaded -rtsopts -o $(PROG)
clean:
	rm -fv *.exe *.o $(PROG)

