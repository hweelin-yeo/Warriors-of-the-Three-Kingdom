play:
	ocamlbuild -use-ocamlfind main.byte && ./main.byte

clean:
	ocamlbuild -clean

