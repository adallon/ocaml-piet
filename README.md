#Ocaml-piet

This repo provides the ocaml-piet tool, which is an ocaml implementation
of the image-based Piet language ( https://www.dangermouse.net/esoteric/piet/ )

The interpreter provides the option to be run in an interactive mode, 
which can be helpful to debug a Piet program.
See options with --help

## Installation
It is recommended to install the dependencies through opam with the following command:
```
$ opam install . -y --deps-only
```
It will download and install dune and imagelib for you.
You can then compile the interpreter through:
```
$ make
```

An executable named ocaml-piet should be available in the current folder.
You can then test the tool through:
```
$ ./ocaml-piet --version
```

## Run

To run the tool on the Piet-hello.png file:
```
$ ./ocaml-piet example/Piet-hello.png
```

To run the tool in interactive mode:
```
$ ./ocaml-piet example/Piet-hello.png -i
```

To print the execution trace in non-interactive mode:
```
$ ./ocaml-piet example/Piet-hello.png -t
```

## On examples
The example folder contains mainly examples found on the internet.

Source (explanations and credits):
https://www.dangermouse.net/esoteric/piet/samples.html
