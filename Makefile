RESULT = myprogram
THREADS = yes
SOURCES = \
  utils.mli utils.ml \
  ustring.mli ustring.ml \
  fmi.ml 
OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
