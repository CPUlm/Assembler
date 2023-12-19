all: asm

asm:
	dune build

test: asm
	@bash test/test.sh _build/default/Assembler.exe

clean:
	dune clean
	rm -f asm

.PHONY: all clean test asm
