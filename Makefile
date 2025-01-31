.PHONY: help test

define DEPS_SCRIPT
(progn
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (package-initialize)
  (package-refresh-contents)
  (package-install 'buttercup)
  (package-install 'consult))
endef
export DEPS_SCRIPT

help:
	@echo "Available commands:"
	@echo "  make deps    Install dependencies"
	@echo "  make test    Run the tests"

deps:
	@echo "Installing dependencies"
	emacs --batch --eval "$$DEPS_SCRIPT"

test:
	emacs --batch --funcall package-initialize --directory . \
	--eval '(add-to-list '\''load-path "..")' \
	--funcall buttercup-run-discover
