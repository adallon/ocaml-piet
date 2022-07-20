all:
	dune build

clean:
	rm -rf _build/

test:
	dune test

exec:
	dune exec ocaml_piet
