export IDRIS2 ?= idris2

.PHONY: page
page:
	pack build docs/docs.js
	cp docs/build/exec/rhonejs.js js/rhonejs.js
