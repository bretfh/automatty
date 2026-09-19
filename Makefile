.PHONY: repl check test test-term run bench latency mux-bench attached compare eval clean

# Two ways to get what cl-vt needs, and every target works under either. Guix is
# what it develops against and what plain `make' uses. FOREIGN=1 is for a mac or
# a linux without guix, where the lisp systems come from wherever asdf already
# finds them. There is nothing to compile either way: cl-vt owns no C.
#
#   make test              guix
#   make FOREIGN=1 test    whatever sbcl is already on this machine
FOREIGN ?=

GUIX := guix shell -m manifest.scm --

ifeq ($(FOREIGN),)
  IN   := $(GUIX) sh -c
  ENV  := CL_SOURCE_REGISTRY="$$PWD//:$$GUIX_ENVIRONMENT/share/common-lisp//" ASDF_OUTPUT_TRANSLATIONS="/:$$HOME/.cache/common-lisp/cl-vt/"
  # --no-userinit: deps come from guix and this tree only; the user's sbclrc
  # must not leak into a cl-vt build.
  SBCL := sbcl --no-userinit --eval "(require :asdf)"
else
  IN   := sh -c
  ENV  := CL_SOURCE_REGISTRY="$$PWD//:$$CL_SOURCE_REGISTRY" ASDF_OUTPUT_TRANSLATIONS="/:$$HOME/.cache/common-lisp/cl-vt/"
  SBCL := sbcl --eval "(require :asdf)"
endif

# what make run puts on the pty, quoted so a shell does not read it first
CMD ?= ls --color=always -la /
CMD_Q = '$(subst ','\'',$(CMD))'

# where make bench and make compare put the corpora they read
BENCH_DIR ?= /tmp/cl-vt-bench
ROUNDS ?= 5

repl:
	$(IN) '$(ENV) $(SBCL) --eval "(asdf:load-system :vt/all)"'

# load everything and say so, without running anything
check:
	$(IN) '$(ENV) $(SBCL) --non-interactive --eval "(asdf:load-system :vt/all)" --eval "(format t \"~&vt loaded, ~D symbols~%\" (let ((n 0)) (do-external-symbols (s :vt) (declare (ignore s)) (incf n)) n))"'

# the fiveam suite. Exits nonzero on failure.
test:
	$(IN) '$(ENV) $(SBCL) --non-interactive --eval "(asdf:test-system :vt/test)"'

# the emulator's own suite, with nothing but the emulator loaded: vt depends on
# nothing and this is what says so
test-term:
	$(IN) '$(ENV) $(SBCL) --non-interactive --eval "(asdf:test-system :vt)"'

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

# the program. ./vt-mux is the whole of it: run it, put it on PATH, copy it to
# another machine. Everything it does is its own argument, not a make target.
vt-mux: build.lisp vt.asd $(wildcard src/*/*.lisp)
	$(IN) '$(ENV) VT_MUX_OUT="$$PWD/vt-mux" $(SBCL) --non-interactive --load build.lisp'

# what a frame costs: a pane blitted to a screen, diffed against what was last
# sent, and encoded as the bytes a terminal reads
mux-bench:
	$(IN) '$(ENV) BENCH_DIR="$(BENCH_DIR)" $(SBCL) --non-interactive --load bench/mux.lisp'

# the same corpora through cl-vt and through tmux, both attached to a terminal
# and both drawing: bytes out, cpu and memory for the same work
attached:
	$(IN) '$(ENV) BENCH_DIR="$(BENCH_DIR)" $(SBCL) --non-interactive --load bench/attached.lisp'

# the same corpora through tmux and through alacritty, under cl-vt's own
# numbers, so the three are read off one screen
compare: bench latency
	$(IN) 'BENCH_DIR="$(BENCH_DIR)" ROUNDS="$(ROUNDS)" sh bench/compare.sh'

# evaluate one form in an image with the test system loaded, in VT/TEST, with
# the debugger left on so a fault prints its backtrace: make eval FORM='(...)'
eval:
	FORM='$(FORM)' $(IN) '$(ENV) $(SBCL) --disable-debugger --eval "(asdf:load-system :vt/test)" --eval "(in-package :vt/test)" --eval "(eval (read-from-string (uiop:getenv \"FORM\")))" --quit'

clean:
	rm -rf $(BENCH_DIR) vt-mux "$$HOME/.cache/common-lisp/cl-vt"
