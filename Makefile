all:
	dune build

noerror:
	dune build --profile release 

clean:
	rm -rf _build/

test:
	dune test --profile release 

exec:
	dune exec ocaml_piet --profile release
