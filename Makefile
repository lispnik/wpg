# Build and test wordperfect-graphics.
#
#   make          build bin/wpg (the command-line driver)
#   make test     run the FiveAM suite
#   make clean    remove bin/ and this tree's fasl cache
#
# Dependencies are vendored in ocicl/ (run `ocicl install` to fetch them).
# The source registry is hermetic -- this tree only -- so a missing
# dependency fails loudly instead of resolving to some other checkout.

SBCL       ?= sbcl
SBCL_FLAGS := --non-interactive --no-userinit --no-sysinit
BOOT       := --eval "(require :asdf)" \
              --eval "(asdf:initialize-source-registry \`(:source-registry (:tree ,(truename \"./\")) :ignore-inherited-configuration))"

SRC := wordperfect-graphics.asd $(wildcard src/*.lisp) $(wildcard cli/*.lisp)
BIN := bin/wpg

.PHONY: all test clean
.DEFAULT_GOAL := all

all: $(BIN)

$(BIN): $(SRC)
	@mkdir -p bin
	$(SBCL) $(SBCL_FLAGS) $(BOOT) \
	  --eval "(asdf:make :wordperfect-graphics/cli)" \
	  --eval "(sb-ext:exit)"
	@ls -lh $(BIN)

test:
	$(SBCL) $(SBCL_FLAGS) $(BOOT) \
	  --eval "(asdf:test-system :wordperfect-graphics)" \
	  --eval "(sb-ext:exit)"

clean:
	rm -rf bin
	rm -rf "$$($(SBCL) --noinform $(SBCL_FLAGS) --eval '(require :asdf)' --eval '(princ (namestring (asdf:apply-output-translations (truename "./"))))')"
