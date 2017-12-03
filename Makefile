test:
	ocamlbuild -use-ocamlfind statetest.byte && ./statetest.byte

play:
	ocamlbuild -use-ocamlfind main.byte && ./main.byte

clean:
	ocamlbuild -clean
