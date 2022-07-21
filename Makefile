all:
	dune build --profile release

error:
	dune build

clean:
	rm -rf _build/

test:
	dune test --profile release 

exec:
	dune exec ocaml_piet --profile release
