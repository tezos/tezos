# You can set these variables from the command line.
SPHINXOPTS    = -aE -n
SPHINXBUILD   = sphinx-build
SPHINXPROJ    = Tezos
SOURCEDIR     = .
BUILDDIR      = _build

DOCGENDIR     = doc_gen
DOCERRORDIR   = $(DOCGENDIR)/errors
DOCRPCDIR     = $(DOCGENDIR)/rpcs

DEV ?= --dev

all: html linkcheck

linkcheck:
	$(SPHINXBUILD) -b linkcheck "$(SOURCEDIR)" "$(BUILDDIR)"

introduction/readme.rst: ../README.rst
	sed 's/TEZOS/How to build and run/' $< > $@

api/errors.rst: $(DOCERRORDIR)/error_doc.ml
	@cd .. && jbuilder build ${DEV} docs/$(DOCERRORDIR)/error_doc.exe
	../_build/default/docs/$(DOCERRORDIR)/error_doc.exe > api/errors.rst

$(DOCGENDIR)/rpc_doc.exe:
	@cd .. && jbuilder build ${DEV} docs/$(DOCGENDIR)/rpc_doc.exe

api/rpc.rst: $(DOCGENDIR)/rpc_doc.exe api/rpc_usage.rst.inc
	@jbuilder exec $(DOCGENDIR)/rpc_doc.exe < api/rpc_usage.rst.inc > api/rpc.rst

$(DOCGENDIR)/p2p_doc.exe:
	@cd .. && jbuilder build ${DEV} docs/$(DOCGENDIR)/p2p_doc.exe

api/p2p.rst: $(DOCGENDIR)/p2p_doc.exe api/p2p_usage.rst.inc
	@jbuilder exec $(DOCGENDIR)/p2p_doc.exe < api/p2p_usage.rst.inc > api/p2p.rst

.PHONY: help Makefile

# Catch-all target: route all unknown targets to Sphinx using the new
# "make mode" option.  $(O) is meant as a shortcut for $(SPHINXOPTS).
html: Makefile api/errors.rst api/rpc.rst api/p2p.rst
	@$(SPHINXBUILD) -b html "$(SOURCEDIR)" "$(BUILDDIR)" $(SPHINXOPTS)

clean:
	@-rm -Rf "$(BUILDDIR)"
	@-rm -Rf introduction/readme.rst api/errors.rst api/rpc.rst
