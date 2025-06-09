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
        (filters '()))

    ;; Content type
    (pcase (transient-arg-value "--type=" args)
      ("story" (push "story" tags))
      ("comment" (push "comment" tags)))

    ;; Author
    (when-let ((author (transient-arg-value "--author=" args)))
      (push (concat "author_" author) tags))

    ;; Search scope
    (when-let ((scope (transient-arg-value "--scope=" args)))
      (push (concat "restrictSearchableAttributes=" scope) filters))

    ;; Time range
    (when-let ((time (transient-arg-value "--time=" args)))
      (unless (string= time "all")
        (let ((seconds (pcase time
                         ("24h" 86400)
                         ("week" (* 7 86400))
                         ("month" (* 30 86400))
                         ("year" (* 365 86400)))))
          (push (format "numericFilters=created_at_i>%d"
                        (- (time-convert nil 'integer) seconds))
                filters))))

    ;; Min points
    (when-let ((points (transient-arg-value "--points=" args)))
      (push (format "numericFilters=points>%s" points) filters))

    ;; Build final query
    (concat query
            (when (or tags filters)
              (concat " -- "
                      (when tags (format "tags=%s" (string-join tags ",")))
                      (when (and tags filters) ",")
                      (string-join filters ","))))))

(defun consult-hn-transient-action ()
  "Execute `consult-hn' with current transient settings."
  (interactive)
  (let ((query (consult-hn-transient--format-query (transient-args 'consult-hn-transient))))
    (consult-hn query)))


(transient-define-prefix consult-hn-transient ()
  "Search Hacker News with filters."
  :value consult-hn-transient-defaults

  ["Search"
   :class transient-column
   ("i" "Query" "--query=" :prompt "Search: ")]

  [["Filters"
    ("t" "Type" "--type="
     :choices ("all" "story" "comment"))
    ("a" "Author" "--author="
     :prompt "Author username: ")]

   ["Scope"
    ("s" "Search in" "--scope="
     :choices (("all" . "all")
               ("url" . "url")
               ("title" . "title")))]

   ["Time"
    ("r" "Range" "--time="
     :choices ("24h" "week" "month" "year" "all"))
    ("p" "Min points" "--points="
     :prompt "Minimum points: "
     :reader transient-read-number-N0)]

   ["Actions"
    ("C-s" "Save as default" consult-hn-transient-save-defaults :transient t)
    ("RET" "Search" consult-hn-transient-action :transient t)]])

(provide 'consult-hn-transient)
;;; consult-hn-transient.el ends here
