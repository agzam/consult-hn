;;; consult-hn-transient.el --- Hacker News search with Consult -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: January 30, 2025
;; Version: 1.1.0
;; Keywords: search extensions
;; Homepage: https://github.com/agzam/consult-hn
;; Package-Requires: ((emacs "29.4") (consult "2.0") (ts "0.3") (transient "0.9"))
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  A menu for composing a Hacker News search before opening it.  An
;;  optional front door: everything here can be done from inside the
;;  session too, through `consult-hn-session-map'.  The menu reads its
;;  values from `consult-hn--params' and writes them back when it
;;  searches, so the two surfaces describe one set of parameters rather
;;  than exchanging a string.
;;
;;; Code:

(require 'transient)
(require 'consult-hn)

(defconst consult-hn-transient--arguments
  '((:query . "--query=")
    (:type . "--type=")
    (:author . "--author=")
    (:points . "--points=")
    (:comments . "--num_comments=")
    (:range . "--time=")
    (:front-page . "--front-page")
    (:url-match . "--url-match")
    (:sort . "--sort="))
  "How each parameter of the model is spelled as a menu argument.")

(defun consult-hn-transient--value (key params)
  "What KEY contributes to the menu under PARAMS, as a string or nil.
The empty string is what a flag with nothing after it comes to."
  (pcase key
    (:query (consult-hn--nonblank (plist-get params :query)))
    (:author (consult-hn--nonblank (plist-get params :author)))
    (:type (pcase (plist-get params :type)
             ('story "story")
             ('comment "comment")))
    (:points (when-let* ((points (plist-get params :points)))
               (number-to-string points)))
    (:comments (when-let* ((comments (plist-get params :comments)))
                 (number-to-string comments)))
    (:range (pcase (plist-get params :range)
              ((or 'all 'nil) nil)
              (range (symbol-name range))))
    (:front-page (and (plist-get params :front-page) ""))
    (:url-match (and (plist-get params :url-match) ""))
    (:sort (pcase (plist-get params :sort)
             ('date "date")
             ('relevance "relevance")))))

(defun consult-hn-transient--args (params)
  "PARAMS as the argument list the menu works in."
  (delq nil
        (mapcar (lambda (cell)
                  (when-let* ((value (consult-hn-transient--value (car cell) params)))
                    (concat (cdr cell) value)))
                consult-hn-transient--arguments)))

(defun consult-hn-transient--arg-value (key args)
  "What ARGS say about KEY, as a string or nil."
  (let ((argument (alist-get key consult-hn-transient--arguments)))
    (when-let* ((arg (seq-find (lambda (a) (string-prefix-p argument a)) args)))
      (substring arg (length argument)))))

(defun consult-hn-transient--params (args)
  "The parameter state ARGS describe."
  (let ((query (consult-hn--nonblank (consult-hn-transient--arg-value :query args))))
    (list :query query
          :type (pcase (consult-hn-transient--arg-value :type args)
                  ("story" 'story)
                  ("comment" 'comment)
                  (_ 'all))
          :author (consult-hn--nonblank
                   (consult-hn-transient--arg-value :author args))
          :points (when-let* ((points (consult-hn--nonblank
                                       (consult-hn-transient--arg-value :points args))))
                    (string-to-number points))
          :comments (when-let* ((comments (consult-hn--nonblank
                                           (consult-hn-transient--arg-value :comments args))))
                      (string-to-number comments))
          :range (pcase (consult-hn-transient--arg-value :range args)
                   ((and (pred stringp) range) (intern range))
                   (_ 'all))
          :front-page (and (consult-hn-transient--arg-value :front-page args) t)
          ;; a pasted URL is a search for who discussed it, which the
          ;; text index answers badly and this restriction answers well
          :url-match (and (or (consult-hn-transient--arg-value :url-match args)
                              (and query (string-match-p "\\`https?://" query)))
                          t)
          :sort (pcase (consult-hn-transient--arg-value :sort args)
                  ("date" 'date)
                  ("relevance" 'relevance)))))

(defun consult-hn-transient--init (key &optional whole)
  "A function seeding KEY's infix from the parameter state.
WHOLE for the classes that hold the whole argument rather than the
value after it."
  (lambda (obj)
    (let ((value (consult-hn-transient--value key consult-hn--params)))
      (oset obj value
            (and value
                 (if whole
                     (concat (alist-get key consult-hn-transient--arguments) value)
                   value))))))

(defun consult-hn-transient--query-init (obj)
  "Seed the query infix OBJ from the region, the symbol at point or the state."
  (oset obj value
        (or (when (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end)))
            (when-let* ((sym (symbol-at-point))) (symbol-name sym))
            (consult-hn--nonblank (plist-get consult-hn--params :query)))))

(transient-define-argument consult-hn-transient--query ()
  :description "Query"
  :class 'transient-option
  :key "i"
  :argument "--query="
  :prompt "Search: "
  :init-value #'consult-hn-transient--query-init)

(transient-define-argument consult-hn-transient--type ()
  :description "Type"
  :class 'transient-switches
  :key "t"
  :argument-format "--type=%s"
  :argument-regexp "--type=\\(story\\|comment\\)"
  :choices '("story" "comment")
  :init-value (consult-hn-transient--init :type t))

(transient-define-argument consult-hn-transient--author ()
  :description "Author"
  :class 'transient-option
  :key "a"
  :argument "--author="
  :prompt "Author username: "
  :init-value (consult-hn-transient--init :author))

(transient-define-argument consult-hn-transient--points ()
  :description "Min points"
  :class 'transient-option
  :key "p"
  :argument "--points="
  :prompt "Minimum points: "
  :reader #'transient-read-number-N0
  :init-value (consult-hn-transient--init :points))

(transient-define-argument consult-hn-transient--comments ()
  :description "Min comments"
  :class 'transient-option
  :key "c"
  :argument "--num_comments="
  :prompt "Minimum number of comments: "
  :reader #'transient-read-number-N0
  :init-value (consult-hn-transient--init :comments))

(transient-define-argument consult-hn-transient--range ()
  :description "Range"
  :class 'transient-switches
  :key "r"
  :argument-format "--time=%s"
  :argument-regexp "--time=\\(24h\\|week\\|month\\|year\\)"
  :choices '("24h" "week" "month" "year")
  :init-value (consult-hn-transient--init :range t))

(transient-define-argument consult-hn-transient--sort ()
  :description "Sort"
  :class 'transient-switches
  :key "s"
  :argument-format "--sort=%s"
  :argument-regexp "--sort=\\(date\\|relevance\\)"
  :choices '("date" "relevance")
  :init-value (consult-hn-transient--init :sort t))

(transient-define-argument consult-hn-transient--front-page ()
  :description "Front page"
  :class 'transient-switch
  :key "f"
  :argument "--front-page"
  :init-value (consult-hn-transient--init :front-page t))

(transient-define-argument consult-hn-transient--url-match ()
  :description "URL matching"
  :class 'transient-switch
  :key "u"
  :argument "--url-match"
  :init-value (consult-hn-transient--init :url-match t))

(defun consult-hn-transient-action ()
  "Search on what the menu says, and keep it as the parameter state.
The session picks the parameters up from there, which is also where it
leaves whatever it is shaped into afterwards."
  (interactive)
  (setq consult-hn--params
        (consult-hn-transient--params (transient-args 'consult-hn-transient)))
  (consult-hn))

(transient-define-prefix consult-hn-transient ()
  "Compose a Hacker News search."
  ["Search"
   :class transient-column
   (consult-hn-transient--query)]

  [["Filters"
    (consult-hn-transient--type)
    (consult-hn-transient--author)
    (consult-hn-transient--front-page)
    (consult-hn-transient--url-match)]

   [""
    (consult-hn-transient--range)
    (consult-hn-transient--points)
    (consult-hn-transient--comments)
    (consult-hn-transient--sort)]

   ["Actions"
    ("RET" "Search" consult-hn-transient-action :transient t)]]

  ;; Graphical Emacs sends <return>, which arrives as RET only by
  ;; translation, and only while nothing else claims it.  A `keymap'
  ;; text property claims it and beats even the menu's own map, which is
  ;; how prompt and chat buffers leave the one action key unreachable.
  [:hide always
   ("<return>" "Search" consult-hn-transient-action :transient t)])

(provide 'consult-hn-transient)
;;; consult-hn-transient.el ends here
