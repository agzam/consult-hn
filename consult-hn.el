;;; consult-hn.el --- Hackernews search with Consult -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: January 30, 2025
;; Modified: January 30, 2025
;; Version: 0.0.1
;; Keywords: search extensions
;; Homepage: https://github.com/agzam/consult-hn
;; Package-Requires: ((emacs "29.4") (consult "2.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'consult)
(require 'subr-x)

(defgroup consult-hn nil
  "Group for consult-hn package."
  :prefix "consult-hn"
  :group 'consult-extensions)

(defcustom consult-hn-default-search-params '((typoTolerance false))
  "Default parameters for consult-hn search."
  :type 'alist
  :group 'consult-hn)

(defvar consult-hn--api-allowed-keys
  '(query
    tags
    numericFilters
    page
    hitsPerPage
    typoTolerance
    restrictSearchableAttributes)
  "Valid fields for HN Algolia API.")

(defun consult-hn--fill-string (str &optional width justify)
  "Fills the STR string with WIDTH and JUSTIFY options."
  (let ((fill-column (or width 50))
        (use-hard-newlines t))
    (with-temp-buffer
      (insert str)
      (fill-region-as-paragraph (point-min) (point-max) (or justify 'left))
      (buffer-string))))

(defun consult-hn--plist->prop-string (plist str-key)
  "Convert PLIST to a propertized string.

STR-KEY is a prop value of the PLIST that will be used for the string
representation, all the rest of the keys will become properties of that
string."
  (apply #'propertize (plist-get plist str-key)
         (cl-loop for (key value) on plist by #'cddr
                  when value
                  collect (intern (substring (symbol-name key) 1))
                  and collect value)))

(defun consult-hn--normalize-input (input)
  "Turn INPUT into a proper query string."
  (when (and input (not (string-blank-p input)))
    (let* ((split (consult--command-split input))
           (params (thread-last
                     (cdr-safe split)
                     (seq-map (lambda (x)
                                (let ((p (split-string x "=")))
                                  (list (intern (car p)) (cadr p)))))
                     (seq-union consult-hn-default-search-params)
                     (seq-filter (lambda (x)
                                   (member (car x) consult-hn--api-allowed-keys)))))
           (_ (setf (alist-get 'query params)
                    (list (url-encode-url (car-safe split)))))
           (params (cl-remove-duplicates params :key #'car)))
      (url-build-query-string params))))

(defun consult-hn--fetch (input cb)
  (let* ((search-url (format "https://hn.algolia.com/api/v1/search_by_date?%s"
                             (consult-hn--normalize-input input)))
         (json-object-type 'hash-table)
         (json-array-type 'list)
         (result (with-current-buffer (url-retrieve-synchronously search-url t t)
                   (goto-char url-http-end-of-headers)
                   (json-read)))
         (rows
          (thread-last
            result
            (gethash "hits")
            (seq-map
             (lambda (x)
               (let* ((author (gethash "author" x))
                      (comment-text (when-let ((comment-markup (gethash "comment_text" x)))
                                      (with-temp-buffer
                                        (insert comment-markup)
                                        (dom-texts (libxml-parse-html-region)))))
                      (title (or (gethash "title" x)
                                 (gethash "story_title" x)))
                      (story-url (gethash "story_url" x))
                      (created-at (gethash "created_at" x))
                      ;; TODO use ts.el for relative dates
                      (hn-base-url "https://news.ycombinator.com/item?id=%s")
                      (hn-story-url (format hn-base-url (gethash "story_id" x)))
                      (object-url (format hn-base-url (gethash "objectID" x))))
                 (list
                  :title title
                  :author author
                  :comment comment-text
                  :created-at created-at
                  :story-url story-url
                  :hn-story-url hn-story-url
                  :hn-object-url object-url)))))))
    (funcall cb rows)))

(defun consult-hn ()
  )

;; (let* ((_ nil))
;;   (consult--read
;;    (consult--async-pipeline
;;     (consult--dynamic-collection #'consult-hn--fetch)
;;     (consult--async-transform (lambda (coll)
;;                                 (thread-last
;;                                   coll
;;                                   (seq-map
;;                                    (lambda (x)
;;                                      (let* ((row (consult-hn--plist->prop-string x :title))
;;                                             (row (truncate-string-to-width row 50 nil nil "..."))
;;                                             (author (plist-get x :author))
;;                                             (created-at (plist-get x :created-at)))
;;                                        (format "%-50s%-40s%30s" row author created-at)))))))
;;     (consult--async-throttle))
;;    :state (lambda (action cand)
;;             (when cand
;;               (print (get-text-property 0 'url cand))
;;               (edebug)))
;;    :prompt "HN Search: "
;;    :annotate (lambda (x)
;;                (format
;;                 "\n%s"
;;                 (consult-hn--fill-string
;;                  (get-text-property 0 'comment x)
;;                  120 'full)))
;;    ))

(provide 'consult-hn)
;;; consult-hn.el ends here
