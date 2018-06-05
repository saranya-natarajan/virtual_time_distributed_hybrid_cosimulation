RESULT = myprogram
THREADS = yes
SOURCES = \
  utils.mli utils.ml \
  ustring.mli ustring.ml \
  fmi.ml components.ml emsoft.ml 
OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
