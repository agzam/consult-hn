;;; consult-hn-e2e-boot.el --- Sandbox bootstrap for the e2e suite -*- lexical-binding: t; -*-

;; Loaded by `make e2e' into a fresh `emacs -nw -Q' running under a PTY.
;; Provides real consult/vertico/orderless from the .elpa-e2e sandbox,
;; loads consult-hn from the repo, and schedules the suite once the
;; frame is up.

;;; Code:

(require 'package)

(let* ((test-dir (file-name-directory load-file-name))
       (root (file-name-directory (directory-file-name test-dir))))
  (setq package-user-dir (expand-file-name ".elpa-e2e" root))
  (package-initialize)
  (add-to-list 'load-path root)
  (add-to-list 'load-path test-dir))

(require 'consult)
(require 'vertico)
(require 'orderless)
(require 'consult-hn)
(require 'consult-hn-e2e)

(vertico-mode 1)
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      vertico-count 15
      consult-async-refresh-delay 0.02
      consult-async-input-debounce 0.01
      consult-async-input-throttle 0.02)

(add-hook 'emacs-startup-hook
          (lambda () (run-at-time 0.2 nil #'consult-hn-e2e-run)))

;;; consult-hn-e2e-boot.el ends here
