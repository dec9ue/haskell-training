PROG=./Factorize.exe
SRC =Factorize.hs

test: $(PROG)
	$(PROG) +RTS -N -s -RTS
$(PROG): $(SRC)
	ghc -O2 $(SRC) -threaded -rtsopts -o $(PROG)
clean:
	rm -fv *.exe *.o $(PROG)

