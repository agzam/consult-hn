;;; consult-hn-transient.el --- Hackernews search with Consult -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: January 30, 2025
;; Version: 1.0.0
;; Keywords: search extensions
;; Homepage: https://github.com/agzam/consult-hn
;; Package-Requires: ((emacs "29.4") (consult "2.0") (ts "0.3") (transient "0.9"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;  This is an extension for Consult
;;  https://github.com/minad/consult
;;
;;; Code:

(require 'transient)
(require 'consult-hn)

(defcustom consult-hn-transient-defaults
  '("--type=all" "--time=all")
  "Default values for `consult-hn' transient."
  :type '(repeat string)
  :group 'consult-hn)

(defun consult-hn-transient-save-defaults ()
  "Save current transient values as defaults."
  (interactive)
  (customize-save-variable 'consult-hn-transient-defaults
                           (transient-args 'consult-hn-transient))
  (message "Defaults saved"))

(defun consult-hn-transient--format-query (args)
  "Convert transient ARGS into HN search query format."
  (let ((query (or (transient-arg-value "--query=" args) ""))
        (tags '())
        (filters '())
        (numeric-filters '())
        (url-matching (member "--url-matching" args)))

    ;; Auto-enable URL matching if query looks like a URL
    (when (and (not url-matching)
               (string-match-p "^https?://" query))
      (setq url-matching t))

    ;; Content type
    (pcase (transient-arg-value "--type=" args)
      ("story" (push "story" tags))
      ("comment" (push "comment" tags)))

    ;; Author
    (when-let ((author (transient-arg-value "--author=" args)))
      (push (concat "author_" author) tags))

    ;; URL matching
    (when url-matching
      (push "restrictSearchableAttributes=url" filters))

    ;; Time range
    (when-let ((time (transient-arg-value "--time=" args)))
      (unless (string= time "all")
        (let ((seconds (pcase time
                         ("24h" 86400)
                         ("week" (* 7 86400))
                         ("month" (* 30 86400))
                         ("year" (* 365 86400)))))
          (push (format "created_at_i>%d"
                        (- (time-convert nil 'integer) seconds))
                numeric-filters))))

    ;; Min points
    (when-let ((points (transient-arg-value "--points=" args)))
      (push (format "points>%s" points) numeric-filters))

    ;; Min comments
    (when-let ((comments (transient-arg-value "--num_comments=" args)))
      (push (format "num_comments>%s" comments) numeric-filters))

    ;; Combine numeric filters into a single parameter
    (when numeric-filters
      (push (format "numericFilters=%s" (string-join numeric-filters ","))
            filters))

    ;; Build final query
    (concat query
            (when (or tags filters)
              (concat " -- "
                      (when tags (format "tags=%s" (string-join tags ",")))
                      (when (and tags filters) " ")
                      (string-join filters " "))))))

(defun consult-hn-transient-action ()
  "Execute `consult-hn' with current transient settings."
  (interactive)
  (let ((query (consult-hn-transient--format-query (transient-args 'consult-hn-transient))))
    (consult-hn query)))

(defun consult-hn-transient--query-reader (prompt initial-input history)
  "Read query input and auto-enable URL matching if needed.
PROMPT is the prompt string.
INITIAL-INPUT is the initial input.
HISTORY is the history variable."
  (let ((query (read-string prompt initial-input history)))
    ;; Auto-enable URL matching if query looks like a URL
    (when (string-match-p "^https?://" query)
      (unless (member "--url-matching" (transient-args 'consult-hn-transient))
        (transient-set-value 'consult-hn-transient "--url-matching" "--url-matching")))
    query))

(transient-define-argument consult-hn-transient--type ()
  :description "Type"
  :class 'transient-switches
  :key "t"
  :argument-format "--type=%s"
  :argument-regexp "--type=\\(story\\|comment\\)"
  :choices '("story" "comment"))

(transient-define-argument consult-hn-transient--range ()
  :description "Range"
  :class 'transient-switches
  :key "r"
  :argument-format "--time=%s"
  :argument-regexp "--time=\\(24h\\|week\\|month\\|year\\)"
  :choices '("24h" "week" "month" "year" "all"))

(transient-define-prefix consult-hn-transient ()
  "Search Hacker News with filters."
  :value consult-hn-transient-defaults

  ["Search"
   :class transient-column
   ("i" "Query" "--query=" 
    :prompt "Search: "
    :reader consult-hn-transient--query-reader)]

  [["Filters"
    (consult-hn-transient--type)
    ("a" "Author" "--author="
     :prompt "Author username: ")
    ("u" "URL matching" "--url-matching")]

   [""
    (consult-hn-transient--range)
    ("p" "Min points" "--points="
     :prompt "Minimum points: "
     :reader transient-read-number-N0)
    ("c" "Min comments" "--num_comments="
     :prompt "Minimum number of comments: "
     :reader transient-read-number-N0)]

   ["Actions"
    ("C-s" "Save as default" consult-hn-transient-save-defaults :transient t)
    ("RET" "Search" consult-hn-transient-action :transient t)]])

(provide 'consult-hn-transient)
;;; consult-hn-transient.el ends here
