name = OCamlChess

all:
	@dune build -j 20
	@rm -f ${name}
	@cp _build/default/main.exe ${name}
	@echo done

clean:
	rm -rvf _build
	@echo CLEAN
