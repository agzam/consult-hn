
.PHONY: help test deps check-compile e2e e2e-deps

# Neither -Q nor --batch relocates `user-emacs-directory': they only skip
# loading init files.  Without --init-directory every unset default - the eln
# cache, auto-save-list, transient's history, package-user-dir - resolves
# against the developer's real ~/.emacs.d and writes there.
SANDBOX := $(CURDIR)/.sandbox
ELPA    := $(CURDIR)/.elpa
EMACS   := emacs --init-directory $(SANDBOX)

define DEPS_SCRIPT
(progn
(require 'package)
(setq package-user-dir (expand-file-name ".elpa"))
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
	@echo "Installing dependencies into .elpa"
	$(EMACS) --batch --eval "$$DEPS_SCRIPT"

# Order-only, so a fresh checkout builds the sandbox once instead of dying
# on `void-function buttercup-run-discover', while a populated one never
# pays for a network refresh.
$(ELPA):
	$(MAKE) deps

test: | $(ELPA)
	$(EMACS) --batch \
	--eval "(setq package-user-dir \"$(ELPA)\")" \
	--funcall package-initialize --directory . \
	--funcall buttercup-run-discover

e2e-deps:
	@echo "Installing e2e sandbox dependencies into .elpa-e2e"
	$(EMACS) --batch --eval "$$E2E_DEPS_SCRIPT"

e2e:
	rm -f test/consult-hn-e2e-results.txt
	TERM=xterm-256color script -q /dev/null $(EMACS) -nw -Q -l test/consult-hn-e2e-boot.el \
	  < /dev/null > /dev/null 2>&1 || true
	@cat test/consult-hn-e2e-results.txt
	@grep -q '^EXIT:0' test/consult-hn-e2e-results.txt

check-compile: deps
	@echo "Checking byte-compilation..."
	$(EMACS) -Q --batch \
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
