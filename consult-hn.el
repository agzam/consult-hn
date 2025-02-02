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
;; Package-Requires: ((emacs "29.4") (consult "2.0") (ts "0.3"))
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
(require 'ts)

(defgroup consult-hn nil
  "Group for consult-hn package."
  :prefix "consult-hn"
  :group 'consult-extensions)

(defcustom consult-hn-default-search-params '((typoTolerance false))
  "Default parameters for consult-hn search."
  :type 'alist
  :group 'consult-hn)

(defcustom consult-hn-preview-story-fn #'consult-hn-preview-story
  "Function pointer for previewing selected HN Story."
  :type 'function
  :group 'consult-hn)

(defcustom consult-hn-preview-comment-fn #'consult-hn-preview-comment
  "Function pointer for previewing selected comment."
  :type 'function
  :group 'consult-hn)

(defcustom consult-hn-browse-story-fn #'consult-hn-browse-story
  "Function pointer for browsing selected HN Story."
  :type 'function
  :group 'consult-hn)

(defcustom consult-hn-browse-comment-fn #'consult-hn-browse-comment
  "Function pointer for browsing selected HN Comment."
  :type 'function
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

(cl-defun consult-hn-preview-story (&key story-url hn-story-url author created-at &allow-other-keys)
  ""
  (let ((after-render (lambda ()
                        (pp eww-data))))
   (remove-hook 'eww-after-render-hook after-render)
   (eww story-url)
   ;; (with-current-buffer (eww story-url :new-buffer)
   ;;   (pp (current-buffer))
   ;;   ;; (edebug)
   ;;   ;; (goto-char (point-min))
   ;;   ;; (read-only-mode -1)
   ;;   ;; (insert (format "HN link: %s\n" hn-story-url))
   ;;   ;; (insert (format "%s | %s\n\n" author created-at))
   ;;   ;; (read-only-mode +1)
   ;;   (display-buffer (current-buffer)))
   ))

(with-current-buffer (eww "https://stackoverflow.com/questions/26102889/how-do-i-make-named-arguments-in-elisp" :new-buffer)
  (save-excursion
    (goto-char (point-min))
    (read-only-mode -1)
    (insert "FOOOHAHHAHA"))
  (display-buffer (current-buffer)))

(cl-defun consult-hn-preview-comment (&key story-url hn-story-url author created-at comment &allow-other-keys)
  (edebug))

(cl-defun consult-hn-browse-story (&key story-url hn-story-url author created-at &allow-other-keys)
  (edebug))

(cl-defun consult-hn-browse-comment (&key story-url hn-story-url author created-at comment &allow-other-keys)
  (edebug))

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

(defun consult-hn--plist-keywordize (plist)
  "Keywordize keys in a PLIST."
  (cl-loop for (k v) on plist by #'cddr
           collect (if (keywordp k) k
                     (intern (concat ":" (symbol-name k))))
           collect v))

(consult-hn--plist-keywordize '(foo "bar" zap "zop"))

(defun consult-hn--time-ago (unix-timestamp)
  "Convert UNIX-TIMESTAMP in the past - into relative time description.
timestamp value must be in utc timezone."
  (let ((diff (- (float-time) unix-timestamp)))
    (cond
     ((<= diff 60) "just now")  ; Changed < to <= to include 0
     (t (concat (car (split-string (ts-human-format-duration diff) ","))
                " ago")))))

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

(defun consult-hn--parse-row-for-lookup (cand-str)
  "Parse displayed candidate string CAND-STR and break into parts."
  (let* ((pattern (rx (group-n 1 (+? any))                 ; title (non-greedy match)
                      (>= 3 space)                         ; separator
                      (group-n 2 (+ (not space)))          ; author
                      (>= 3 space)                         ; separator
                      (group-n 3 (+? any))                 ; ago
                      (>= 3 space)                         ; separator
                      (group-n 4 (+ (not space)))          ; created-at
                      (optional (>= 3 space)               ; comment
                                (group-n 5 (+ not-newline))))))
    (when (string-match pattern cand-str)
      (list :title (match-string 1 cand-str)
            :author (match-string 2 cand-str)
            :created-at (match-string 4 cand-str)
            :comment (match-string 5 cand-str)))))

(defun consult-hn--fetch (input cb)
  ""
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
                      (comment-text (when-let* ((comment-markup (gethash "comment_text" x)))
                                      (with-temp-buffer
                                        (insert comment-markup)
                                        (dom-texts (libxml-parse-html-region)))))
                      (title (or (gethash "title" x)
                                 (gethash "story_title" x)))
                      (story-url (or (gethash "story_url" x)
                                     (gethash "url" x)))
                      (created-at (gethash "created_at" x))
                      (ts (gethash "created_at_i" x))
                      ;; TODO use ts.el for relative dates
                      (hn-base-url "https://news.ycombinator.com/item?id=%s")
                      (hn-story-url (format hn-base-url (gethash "story_id" x)))
                      (object-url (format hn-base-url (gethash "objectID" x))))
                 (list
                  :title (replace-regexp-in-string " +" " " title) ; titles shouldn't have two or more spaces
                  :author author
                  :comment comment-text
                  :created-at created-at
                  :story-url story-url
                  :hn-story-url hn-story-url
                  :hn-object-url object-url
                  :ts ts)))))))
    (funcall cb rows)))

(defun consul-hn--async-transform (coll)
  "Transform COLL function."
  (thread-last
    coll
    (seq-map
     (lambda (x)
       (let* ((row (consult-hn--plist->prop-string x :title))
              (row (truncate-string-to-width row 69 nil nil "..."))
              (_ (add-text-properties 0 (length row) '(face bold) row)) ; title is bold
              (author (plist-get x :author))
              (created-at (propertize (plist-get x :created-at)
                                      'invisible t))
              (ago (consult-hn--time-ago (plist-get x :ts)))
              ;; add comment to the row, but hide it
              ;; this is a trick to make the comments "filterable"
              (comment (if-let* ((cmt (plist-get x :comment)))
                           (propertize cmt 'invisible t) "")))
         (format "%-75s   %-20s   %20s   %s   %s"
                 row author ago created-at comment))))))

(defun consult-hn--async-lookup (cand coll _input _narr)
  "Lookup fn."
  (let* ((parsed (consult-hn--parse-row-for-lookup cand))
         (created-at (plist-get parsed :created-at))
         (title (plist-get parsed :title))
         (comment (plist-get parsed :comment)))
    (thread-last
      coll
      (seq-find (lambda (row)
                  (let* ((props (text-properties-at 0 row))
                         (r-created-at (plist-get props 'created-at))
                         (r-title (plist-get props 'title))
                         (r-comment (plist-get props 'comment)))
                    (and
                     (string= created-at r-created-at)
                     (string-prefix-p (replace-regexp-in-string "\\.{3}$" "" title)
                                      r-title)
                     (string= comment r-comment))))))))

(defun consult-hn ()
  ""
  (interactive)
  (consult--read
   (consult--async-pipeline
    (consult--dynamic-collection #'consult-hn--fetch)
    (consult--async-transform #'consul-hn--async-transform)
    (consult--async-throttle))
   :lookup #'consult-hn--async-lookup
   :state (lambda (action cand)
            (when-let* ((_ cand)
                        (props (consult-hn--plist-keywordize
                                (text-properties-at 0 cand))))
              (cond
               ((eq action 'preview)
                (if (plist-get props :comment)
                    (apply consult-hn-preview-comment-fn props)
                  (apply consult-hn-preview-story-fn props)))
               ((eq action 'return)
                (if (plist-get props :comment)
                    (apply consult-hn-browse-comment-fn props)
                  (apply consult-hn-browse-story-fn props))))))
   :prompt "HN Search: "
   :annotate (lambda (x)
               ;; comments shown as annotation
               (if-let* ((comment (get-text-property 0 'comment x))
                         (ann-txt (replace-regexp-in-string
                                   "^" "  " ; prefix every line in the comment with an indent
                                   (consult-hn--fill-string comment 120 'full))))
                   (format "\n%s\n" ann-txt)
                 "\n"))))

(provide 'consult-hn)
;;; consult-hn.el ends here
