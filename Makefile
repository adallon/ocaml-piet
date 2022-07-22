all:
	dune build 

noerror:
	dune build --profile release

clean:
	rm -rf _build/

test:
	dune test

exec:
	dune exec ocaml_piet
