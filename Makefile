all:
	ocamlfind ocamlopt -package batteries -linkpkg -annot -g -c \
        interval_tree.ml

clean:
	\rm -f *.o *.cmi *.cmx *.annot
