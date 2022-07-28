all:
	dune build 
	cp _build/default/bin/main.exe ocaml-piet.exe

noerror:
	dune build --profile release

clean:
	rm -rf _build/
	rm -f ocaml-piet.exe

test:   _build/default/bin/main.exe
	_build/default/bin/main.exe examples/Piet_hello.png
	_build/default/bin/main.exe examples/Piet_hello2.png

exec:
	dune exec ocaml_piet
