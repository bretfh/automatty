.PHONY: libs repl check test run bench compare eval clean

# Two ways to get what cl-vt needs, and every target works under either. Guix is
# what it develops against and what plain `make' uses. FOREIGN=1 is for a mac or
# a linux without guix: the lisp systems come from wherever asdf already finds
# them, the C from the system's own compiler.
#
#   make test              guix
#   make FOREIGN=1 test    the toolchain already on this machine
FOREIGN ?=

GUIX := guix shell -m manifest.scm --

ifeq ($(FOREIGN),)
  # gcc-toolchain, from the manifest; the host's cc is not what is in here
  BUILD_CC = gcc
  IN   := $(GUIX) sh -c
  ENV  := LD_LIBRARY_PATH="$$GUIX_ENVIRONMENT/lib" CL_SOURCE_REGISTRY="$$PWD//:$$GUIX_ENVIRONMENT/share/common-lisp//" ASDF_OUTPUT_TRANSLATIONS="/:$$HOME/.cache/common-lisp/cl-vt/"
  # --no-userinit: deps come from guix and this tree only; the user's sbclrc
  # must not leak into a cl-vt build.
  SBCL := sbcl --no-userinit --eval "(require :asdf)"
else
  BUILD_CC = $(CC)
  IN   := sh -c
  ENV  := CL_SOURCE_REGISTRY="$$PWD//:$$CL_SOURCE_REGISTRY" ASDF_OUTPUT_TRANSLATIONS="/:$$HOME/.cache/common-lisp/cl-vt/"
  SBCL := sbcl --eval "(require :asdf)"
endif

LIBDIR := lib

# what make run puts on the pty, quoted so a shell does not read it first
CMD ?= ls --color=always -la /
CMD_Q = '$(subst ','\'',$(CMD))'

# where make bench and make compare put the corpora they read
BENCH_DIR ?= /tmp/cl-vt-bench
ROUNDS ?= 5


CC := $(shell command -v cc 2>/dev/null || command -v gcc 2>/dev/null)

UNAME := $(shell uname -s)
ifeq ($(UNAME),Darwin)
  # openpty is in libc on a mac; on linux it is in libutil
  PTY_LINK :=
  SOEXT := dylib
else
  PTY_LINK := -lutil
  SOEXT := so
endif

# The C cl-vt owns: the shim that puts a program on a pty of its own. The guix
# package builds this into its output; a tree you run from builds it here, once,
# and vt/pty looks for it beside the system it was loaded from.
libs:
	@test -f $(LIBDIR)/libvt-pty.$(SOEXT) \
	  || $(IN) 'mkdir -p $(LIBDIR) && \
	    $(BUILD_CC) -O2 c/pty-helper.c -o $(LIBDIR)/vt-pty-helper && \
	    $(BUILD_CC) -shared -fPIC -DVT_PTY_HELPER=\"$(CURDIR)/$(LIBDIR)/vt-pty-helper\" \
	       c/pty.c -o $(LIBDIR)/libvt-pty.$(SOEXT) $(PTY_LINK) && \
	    echo "built $(LIBDIR)"'

repl: libs
	$(IN) '$(ENV) $(SBCL) --eval "(asdf:load-system :vt/all)"'

# load everything and say so, without running anything
check: libs
	$(IN) '$(ENV) $(SBCL) --non-interactive --eval "(asdf:load-system :vt/all)" --eval "(format t \"~&vt loaded, ~D symbols~%\" (let ((n 0)) (do-external-symbols (s :vt) (declare (ignore s)) (incf n)) n))"'

# the fiveam suite. Exits nonzero on failure.
test: libs
	$(IN) '$(ENV) $(SBCL) --non-interactive --eval "(asdf:test-system :vt)"'

# run a command on a pty and print the screen it drew:
#   make run CMD='top -b -n 1'
run: libs
	CMD=$(CMD_Q) $(IN) '$(ENV) $(SBCL) --non-interactive --load examples/screen.lisp'

# how many bytes a second it reads, over plain, coloured and redrawing output,
# through the parser and through a real pty. BENCH_DIR is where the corpora go.
bench: libs
	$(IN) '$(ENV) BENCH_DIR="$(BENCH_DIR)" $(SBCL) --non-interactive --load bench/throughput.lisp'

# the same corpora through tmux and through alacritty, under cl-vt's own
# numbers, so the three are read off one screen
compare: bench
	$(IN) 'BENCH_DIR="$(BENCH_DIR)" ROUNDS="$(ROUNDS)" sh bench/compare.sh'

# evaluate one form in an image with the test system loaded, in VT/TEST, with
# the debugger left on so a fault prints its backtrace: make eval FORM='(...)'
eval: libs
	FORM='$(FORM)' $(IN) '$(ENV) $(SBCL) --disable-debugger --eval "(asdf:load-system :vt/test)" --eval "(in-package :vt/test)" --eval "(eval (read-from-string (uiop:getenv \"FORM\")))" --quit'

clean:
	rm -rf $(LIBDIR) $(BENCH_DIR) "$$HOME/.cache/common-lisp/cl-vt"
