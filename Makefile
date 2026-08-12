
.PHONY: help test deps check-compile e2e e2e-deps

define DEPS_SCRIPT
(progn
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)
(package-install 'buttercup)
(package-install 'consult)
(package-install 'ts))
endef
export DEPS_SCRIPT

# Deliberately a separate sandbox from .elpa: the unit suite must keep
# proving the package works without vertico or orderless installed.
define E2E_DEPS_SCRIPT
(progn
(require 'package)
(setq package-user-dir (expand-file-name ".elpa-e2e"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)
(dolist (p '(consult vertico orderless ts))
(unless (package-installed-p p) (package-install p))))
endef
export E2E_DEPS_SCRIPT

help:
	@echo "Available commands:"
	@echo "  make deps          Install dependencies"
	@echo "  make test          Run the tests"
	@echo "  make compile       Byte-compile the package"
	@echo "  make check-compile Check for clean byte-compilation"

deps:
	@echo "Installing dependencies"
	emacs --batch --eval "$$DEPS_SCRIPT"

test:
	emacs --batch --funcall package-initialize --directory . \
	--eval '(add-to-list '\''load-path "..")' \
	--funcall buttercup-run-discover

e2e-deps:
	@echo "Installing e2e sandbox dependencies into .elpa-e2e"
	emacs --batch --eval "$$E2E_DEPS_SCRIPT"

e2e:
	rm -f test/consult-hn-e2e-results.txt
	TERM=xterm-256color script -q /dev/null emacs -nw -Q -l test/consult-hn-e2e-boot.el \
	  < /dev/null > /dev/null 2>&1 || true
	@cat test/consult-hn-e2e-results.txt
	@grep -q '^EXIT:0' test/consult-hn-e2e-results.txt

check-compile: deps
	@echo "Checking byte-compilation..."
	emacs -Q --batch \
	--eval "(require 'package)" \
	--eval "(setq package-user-dir \"$(CURDIR)/.elpa\")" \
	--eval "(add-to-list 'package-archives '(\"melpa\" . \"http://melpa.org/packages/\"))" \
	--eval "(package-initialize)" \
	--eval "(package-install 'consult)" \
	--eval "(package-install 'ts)" \
	--eval "(setq byte-compile-error-on-warn t)" \
	--eval "(add-to-list 'load-path \".\")" \
	--eval "(byte-compile-file \"consult-hn.el\")" \
    --eval "(byte-compile-file \"consult-hn-transient.el\")"
