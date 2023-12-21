all: asm

asm:
	@dune build bin/main.exe
	@cp -f _build/default/bin/main.exe asm

doc: doc/asm.tex doc/stack.tex
	cd doc && latexmk asm.tex

test: asm
	@bash test/test.sh ./asm

clean:
	dune clean
	rm -f asm
	cd doc && latexmk -c asm.tex

.PHONY: all clean test asm doc
