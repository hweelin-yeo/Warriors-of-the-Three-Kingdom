test:
	ocamlbuild -use-ocamlfind statetest.byte && ./statetest.byte
	ocamlbuild -use-ocamlfind aiTest.byte && ./aiTest.byte
	ocamlbuild -use-ocamlfind PlayerStateTest.byte && ./PlayerStateTest.byte

play:
	ocamlbuild -use-ocamlfind main.byte && ./main.byte

clean:
	ocamlbuild -clean
