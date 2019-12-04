vm: vm.hs
	ghc -outputdir build $(GHCFLAGS) -O2 -o $@ vm.hs

.PHONY: clean
clean:
	rm -rf build vm
