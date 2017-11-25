test:
<<<<<<< HEAD

	ocamlbuild 
=======
	ocamlbuild -use-ocamlfind state_test.byte && ./state_test.byte

play:
	ocamlbuild -use-ocamlfind main.byte && ./main.byte

clean:
	ocamlbuild -clean

>>>>>>> 895696332890b698e5d84a76184c5e18018b8d70
