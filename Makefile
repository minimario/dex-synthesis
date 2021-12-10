main:
	ocamlbuild -use-ocamlfind synthesizer.byte -tag thread
	./synthesizer.byte

clean:
	rm -f synthesizer.byte