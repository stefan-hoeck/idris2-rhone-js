export IDRIS2 ?= idris2

lib_pkg = rhone-js.ipkg

doc_pkg = doc.ipkg

.PHONY: all
all: lib doc

.PHONY: clean-install
clean-install: clean install

.PHONY: clean-install-with-src
clean-install-with-src: clean install

.PHONY: lib
lib:
	${IDRIS2} --build ${lib_pkg}

.PHONY: doc
doc:
	${IDRIS2} --build ${doc_pkg}

.PHONY: page
page: doc
	cp build/exec/rhonejs.js js/rhonejs.js

.PHONY: install
install:
	${IDRIS2} --install ${lib_pkg}

.PHONY: install-with-src
install-with-src:
	${IDRIS2} --install-with-src ${lib_pkg}

.PHONY: clean
clean:
	${IDRIS2} --clean ${lib_pkg}
	${IDRIS2} --clean ${doc_pkg}
	${RM} -r build

.PHONY: develop
develop:
	find -regextype posix-extended -regex ".*\.(idr|md)" | entr -d idris2 --typecheck ${lib_pkg}

.PHONY: develop-doc
develop-doc:
	find -regextype posix-extended -regex ".*\.(idr|md)" | entr -d idris2 --typecheck ${doc_pkg}
