all:
	ocamlfind ocamlopt -annot -g -c interval_tree.ml

clean:
	\rm -f *.o *.cmi *.cmx *.annot
