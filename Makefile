.PHONY: repl check deps sbcl sbcl-bin test test-term run bench latency mux-bench attached compare eval release clean install uninstall

# Two ways to get what atty needs, and every target works under either. Guix is
# what it develops against and what plain `make' uses: manifest.scm names the
# lisp systems. FOREIGN=1 is for a mac or a linux without guix: ocicl.csv pins
# the same systems and `make deps' fetches them into ./ocicl, with curl and tar
# (deps.lisp; the ocicl program itself is only for changing what is pinned).
# Either way sbcl runs without the user's init file and sees exactly this
# directory and the one place the dependencies are, so what is on a machine's
# sbclrc or in its home never reaches a build. There is nothing to compile
# either way: atty owns no C.
#
#   make test                       guix
#   make FOREIGN=1 deps test        whatever sbcl is on this machine
FOREIGN ?=

GUIX := guix shell -m manifest.scm --

# asdf reads this directory for atty.asd and libatty.asd, not the tree under
# it: what is under ocicl/ or dist/ is not atty's to find by itself
REGISTRY = (:source-registry (:directory \"$$PWD/\") $(DEP_TREE) :ignore-inherited-configuration)

# which sbcl runs the build, and where what it compiles goes: one place per
# sbcl, since a fasl is only good for the sbcl that wrote it
SBCL_BIN ?= sbcl
FASL_DIR ?= $$HOME/.cache/common-lisp/atty/

ifeq ($(FOREIGN),)
  IN   := $(GUIX) sh -c
  DEP_TREE := (:tree \"$$GUIX_ENVIRONMENT/share/common-lisp/\")
else
  IN   := sh -c
  DEP_TREE := (:tree \"$$PWD/ocicl/\")
endif
SBCL := $(SBCL_BIN) --no-userinit --eval "(require :asdf)"
ENV := CL_SOURCE_REGISTRY="$(REGISTRY)" ASDF_OUTPUT_TRANSLATIONS="/:$(FASL_DIR)"

# the build says which commit it is; a tag makes that a version
VERSION := $(shell git describe --tags --always --dirty 2>/dev/null || echo unknown)
OS      := $(shell uname -s | tr A-Z a-z)
ARCH    := $(shell uname -m)
SHA256  := $(shell command -v sha256sum >/dev/null 2>&1 && echo sha256sum || echo "shasum -a 256")

# The sbcl a release is built with: roswell's build of it for this platform,
# pinned. save-lisp-and-die puts the runtime of the sbcl that runs it into
# atty, with whatever that runtime needs; brew's needs brew's zstd and the
# macOS it was built on. roswell's link nothing but the system and run on old
# systems: glibc 2.17 on linux, macOS 14 on arm and 10.9 on intel.
SBCL_VERSION  := 2.6.8
SBCL_ARCH     := $(if $(filter arm64 aarch64,$(ARCH)),arm64,x86-64)
SBCL_PLATFORM := $(SBCL_ARCH)-$(if $(filter darwin,$(OS)),darwin,linux-glibc2.19)
SBCL_ROOT     := $(CURDIR)/sbcl/$(SBCL_VERSION)
PINNED_SBCL   := $(SBCL_ROOT)/bin/sbcl

# what make run puts on the pty, quoted so a shell does not read it first
CMD ?= ls --color=always -la /
CMD_Q = '$(subst ','\'',$(CMD))'

# where make bench and make compare put the corpora they read
BENCH_DIR ?= /tmp/atty-bench
ROUNDS ?= 5

repl:
	$(IN) '$(ENV) $(SBCL) --eval "(asdf:load-system :atty/all)"'

# the lisp systems atty needs, where this build looks for them. Under guix
# they are the manifest's and there is nothing to do. Otherwise deps.lisp
# fetches what ocicl.csv pins into ./ocicl.
deps:
ifeq ($(FOREIGN),)
	@echo "guix has them: manifest.scm"
else
	$(SBCL_BIN) --no-userinit --non-interactive --load deps.lisp
endif

# the pinned sbcl, in ./sbcl/<version>; sbcl-bin says where, for a PATH
sbcl: $(PINNED_SBCL)

$(PINNED_SBCL):
	mkdir -p sbcl
	curl -fsSL https://github.com/roswell/sbcl_bin/releases/download/$(SBCL_VERSION)/sbcl-$(SBCL_VERSION)-$(SBCL_PLATFORM)-binary.tar.bz2 | tar xj -C sbcl
	cd sbcl/sbcl-$(SBCL_VERSION)-$(SBCL_PLATFORM) && INSTALL_ROOT="$(SBCL_ROOT)" sh install.sh >/dev/null
	rm -rf sbcl/sbcl-$(SBCL_VERSION)-$(SBCL_PLATFORM)
	$(PINNED_SBCL) --version

sbcl-bin:
	@echo "$(SBCL_ROOT)/bin"

# load everything and say so, without running anything
check:
	$(IN) '$(ENV) $(SBCL) --non-interactive --eval "(asdf:load-system :atty/all)" --eval "(format t \"~&libatty loaded, ~D symbols~%\" (let ((n 0)) (do-external-symbols (s :libatty) (declare (ignore s)) (incf n)) n))"'

# the fiveam suite. Exits nonzero on failure.
test:
	$(IN) '$(ENV) $(SBCL) --non-interactive --eval "(asdf:test-system :atty/test)"'

# the emulator's own suite, with nothing but the emulator loaded: libatty
# depends on nothing and this is what says so
test-term:
	$(IN) '$(ENV) $(SBCL) --non-interactive --eval "(asdf:test-system :libatty)"'

# run a command on a pty and print the screen it drew:
#   make run CMD='top -b -n 1'
run:
	CMD=$(CMD_Q) $(IN) '$(ENV) $(SBCL) --non-interactive --load examples/screen.lisp'

# how many bytes a second it reads, over plain, coloured and redrawing output,
# through the parser and through a real pty. BENCH_DIR is where the corpora go.
bench:
	$(IN) '$(ENV) BENCH_DIR="$(BENCH_DIR)" $(SBCL) --non-interactive --load bench/throughput.lisp'

# what one read costs, how long a collection stops it, and what it weighs
latency:
	$(IN) '$(ENV) BENCH_DIR="$(BENCH_DIR)" $(SBCL) --non-interactive --load bench/latency.lisp'

# the program. ./atty is the whole of it: run it, put it on PATH, copy it to
# another machine. Everything it does is its own argument, not a make target.
atty: build.lisp atty.asd libatty.asd $(wildcard src/*/*.lisp src/*/*/*.lisp src/*/*/*/*.lisp) $(wildcard readers/*/*/reader.lisp)
	$(IN) '$(ENV) ATTY_OUT="$$PWD/atty" ATTY_VERSION="$(VERSION)" $(SBCL) --non-interactive --load build.lisp'

# the binary as a release carries it: one tarball named for the version and
# the platform, and the sum to check it by. A tag workflow uploads these.
release: $(PINNED_SBCL)
	$(MAKE) -B atty SBCL_BIN="$(PINNED_SBCL)" FASL_DIR="$(CURDIR)/sbcl/fasl/"
	rm -rf dist
	mkdir -p dist
	tar czf dist/atty-$(VERSION)-$(OS)-$(ARCH).tar.gz atty
	cd dist && $(SHA256) atty-$(VERSION)-$(OS)-$(ARCH).tar.gz > SHA256SUMS
	@echo "dist/atty-$(VERSION)-$(OS)-$(ARCH).tar.gz"

# builds atty and puts it on PATH. PREFIX defaults to /usr/local, which usually
# wants root; PREFIX=$$HOME/.local avoids that if that is already on PATH.
PREFIX ?= /usr/local

install: atty
	install -d "$(DESTDIR)$(PREFIX)/bin"
	install -m 755 atty "$(DESTDIR)$(PREFIX)/bin/atty"

uninstall:
	rm -f "$(DESTDIR)$(PREFIX)/bin/atty"

# what a frame costs: a pane blitted to a screen, diffed against what was last
# sent, and encoded as the bytes a terminal reads
mux-bench:
	$(IN) '$(ENV) BENCH_DIR="$(BENCH_DIR)" $(SBCL) --non-interactive --load bench/mux.lisp'

# the same corpora through atty and through tmux, both attached to a terminal
# and both drawing: bytes out, cpu and memory for the same work. Against the
# built binary, not a fresh SBCL loading ASDF: that is not what runs it.
attached: atty
	$(IN) '$(ENV) BENCH_DIR="$(BENCH_DIR)" $(SBCL) --non-interactive --load bench/attached.lisp'

# the same corpora through tmux and through alacritty, under atty's own
# numbers, so the three are read off one screen
compare: bench latency
	$(IN) 'BENCH_DIR="$(BENCH_DIR)" ROUNDS="$(ROUNDS)" sh bench/compare.sh'

# evaluate one form in an image with the test system loaded, in ATTY/TEST, with
# the debugger left on so a fault prints its backtrace: make eval FORM='(...)'
eval:
	FORM='$(FORM)' $(IN) '$(ENV) $(SBCL) --disable-debugger --eval "(asdf:load-system :atty/test)" --eval "(in-package :atty/test)" --eval "(eval (read-from-string (uiop:getenv \"FORM\")))" --quit'

clean:
	rm -rf $(BENCH_DIR) atty dist "$$HOME/.cache/common-lisp/atty"
