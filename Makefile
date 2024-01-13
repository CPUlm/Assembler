all: asm
sources  := $(shell find . -regextype sed -regex "\./\(lib\|bin\)/.*\|\./dune-project")

asm: $(sources)
	@dune build bin/main.exe
	@cp -f _build/default/bin/main.exe asm

doc: doc/asm.tex doc/stack.tex
	cd doc && latexmk asm.tex

test: asm
	@bash test/test.sh ./asm

clean:
	rm -rf _build/ asm
	cd doc && latexmk -c asm.tex

dev:
	dune build -w

.PHONY: all clean test asm doc
