FRAMAC_SHARE:=$(shell frama-c-config -print-share-path)

PLUGIN_NAME = acsl_lsp


## Common definitions
#include ${FRAMAC_SHARE}/Makefile.common
## Tests-related targets
#include ${FRAMAC_SHARE}/Makefile.testing
## Installation-related targets
#include ${FRAMAC_SHARE}/Makefile.installation

.PHONY: build
build:
	dune build

.PHONY: test
test: build src/testing/server/node_modules
	cd src/testing/server && npm test