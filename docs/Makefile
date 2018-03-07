# You can set these variables from the command line.
SPHINXOPTS    = -aE -n
SPHINXBUILD   = sphinx-build
SPHINXPROJ    = Tezos
SOURCEDIR     = .
BUILDDIR      = _build

DOCGENDIR     = doc_gen
DOCERRORDIR   = $(DOCGENDIR)/errors
DOCRPCDIR   = $(DOCGENDIR)/rpcs

all: html linkcheck

linkcheck:
	$(SPHINXBUILD) -b linkcheck "$(SOURCEDIR)" "$(BUILDDIR)"

api/errors.rst: $(DOCERRORDIR)/error_doc.ml
	@jbuilder build $(DOCERRORDIR)/error_doc.exe
	../_build/default/docs/$(DOCERRORDIR)/error_doc.exe > api/errors.rst

api/rpc.rst: $(DOCRPCDIR)/rpc_doc.ml $(DOCRPCDIR)/usage.rst $(DOCRPCDIR)/run_rpc_doc.sh
	@jbuilder build $(DOCRPCDIR)/rpc_doc.exe
	./$(DOCRPCDIR)/run_rpc_doc.sh > api/rpc.rst

.PHONY: help Makefile

# Catch-all target: route all unknown targets to Sphinx using the new
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
html: Makefile api/errors.rst api/rpc.rst
	@$(SPHINXBUILD) -b html "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS)

clean:
	@-rm -Rf "$(BUILDDIR)"
	@-rm -Rf introduction/readme.rst api/errors.rst
