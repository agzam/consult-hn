;;; consult-hn.el --- Hackernews search with Consult -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: January 30, 2025
;; Version: 1.0.0
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
;;  This is an extension for Consult
;;  https://github.com/minad/consult
;;
;;; Code:

(require 'consult)
(require 'subr-x)
(require 'ts)
(require 'url)
(require 'url-http)
(require 'json)

(defgroup consult-hn nil
  "Group for `consult-hn' package."
  :prefix "consult-hn"
  :group 'consult-extensions)

(defcustom consult-hn-default-search-params '((typoTolerance false))
  "Default parameters for `consult-hn' search."
  :type 'alist
  :group 'consult-hn)

(defcustom consult-hn-initial-input-string ""
  "Initial input string."
  :type 'string
  :group 'consult-hn)

(defcustom consult-hn-preview-fn #'consult-hn-eww
  "Function pointer for browsing selected HN Story."
  :type 'function
  :group 'consult-hn)

(defcustom consult-hn-browse-fn #'consult-hn-eww
  "Function pointer for browsing selected HN Story."
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

(defvar consult-hn--history nil
  "History of queries for `consult-hn'.")

(cl-defun consult-hn-eww (&key story-url title hn-story-url author created-at hn-object-url num-comments points comment &allow-other-keys)
  "Open hackernews item in eww buffer.
STORY-URL TITLE HN-STORY-URL AUTHOR CREATED-AT
HN-OBJECT-URL NUM-COMMENTS POINTS COMMENT - are all the HN-relevant things."
  (cl-labels ((after-render-a (ofn status url &optional point buffer encode)
                (unwind-protect
                    (progn
                      (funcall ofn status url point buffer encode)
                      (with-current-buffer buffer
                        (read-only-mode -1)
                        (goto-char (point-min))
                        (shr-insert-document
                         (with-temp-buffer
                           (insert "<div>")
                           (insert (format "<div><a href=\"%s\">%s</a></div>" story-url title))
                           (when comment
                            (insert (format "<div><a href=\"%s\">%s</a></div>" hn-story-url hn-story-url)))
                           (insert "<div><span>")
                           (insert (format "<a href=\"%s\">%s</a>"
                                           (concat "https://news.ycombinator.com/user?id=" author)
                                           author))
                           (insert (format " | <a href=\"%s\">%s</a>"
                                           hn-object-url
                                           (consult-hn--time-ago (ts-unix (ts-parse created-at)))))
                           (when points (insert (format " | %s points" points)))
                           (when num-comments (insert (format " | %s comments" num-comments)))
                           (insert "</span></div>")
                           (insert "</div>")
                           (insert "<hr/><br/><br/>")
                           (libxml-parse-html-region)))))
                 (advice-remove 'eww-render #'after-render-a))))
    (advice-add 'eww-render :around #'after-render-a)
    (eww (or story-url hn-story-url) :new-buffer)))

(defun consult-hn--fill-string (str &optional width justify)
  "Fills the STR string with WIDTH and JUSTIFY options."
  (let ((fill-column (or width 50))
        (use-hard-newlines t))
    (with-temp-buffer
      (insert str)
      (fill-region-as-paragraph (point-min) (point-max) (or justify 'left))
      (buffer-string))))

(defun consult-hn--plist-keywordize (plist)
  "Keywordize keys in a PLIST."
  (cl-loop for (k v) on plist by #'cddr
           collect (if (keywordp k) k
                     (intern (concat ":" (symbol-name k))))
           collect v))

(defun consult-hn--time-ago (unix-timestamp)
  "Convert UNIX-TIMESTAMP in the past - into relative time description.
timestamp value must be in utc timezone."
  (let ((diff (- (float-time) unix-timestamp)))
    (cond
     ((<= diff 60) "just now")  ; Changed < to <= to include 0
     (t (concat (car (split-string (ts-human-format-duration diff) ","))
                " ago")))))

(defun consult-hn--input->params (input)
  "Turn INPUT into a proper query string."
  (when (and input (not (string-blank-p input)))
    (let* ((split (split-string input "--" nil " +"))
           (params (thread-last
                     (when-let* ((parts (cadr split))
                                 (parts (split-string parts "=" nil " +")))
                       (cl-loop for (k v) on parts by #'cddr
                                when (and k v
                                          (not (string-blank-p k))
                                          (not (string-blank-p v)))
                                collect (list (intern k) (replace-regexp-in-string " +" "" v))))
                     (seq-union consult-hn-default-search-params)
                     (seq-filter (lambda (x)
                                   (member (car x) consult-hn--api-allowed-keys)))))
           (_ (unless (string-blank-p (car-safe split))
                (setf (alist-get 'query params)
                      (list (url-encode-url (car-safe split))))))
           (params (cl-remove-duplicates params :key #'car)))
      params)))

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

(defun consult-hn--async-transform (coll)
  "Transform COLL function."
  (thread-last
    coll
    (seq-map
     (lambda (x)
       (let* ((row (truncate-string-to-width x 69 nil nil "..."))
              (_ (add-text-properties 0 (length row) '(face bold) row)) ; title is bold
              (author (get-text-property 0 'author x))
              (ago (consult-hn--time-ago (get-text-property 0 'ts x)))
              ;; add comment and created at to the row, but hide them
              ;; this is a trick to make the comments "filterable"
              (created-at (propertize (get-text-property 0 'created-at x)
                                      'invisible t))
              (comment (if-let* ((cmt (get-text-property 0 'comment x)))
                           (propertize cmt 'invisible t) "")))
         (format "%-75s   %-20s   %20s   %s   %s"
                 row author ago created-at comment))))))

(defun consult-hn--async-lookup (cand coll _input _narr)
  "Lookup fn. CAND and COLL standard `consult--read' args for :lookup key."
  (when (and cand coll)
    (let* ((parsed (consult-hn--parse-row-for-lookup cand))
           (created-at (plist-get parsed :created-at))
           (title (plist-get parsed :title))
           (found (thread-last
                    coll
                    (seq-find
                     (lambda (row)
                       (let* ((props (text-properties-at 0 row))
                              (r-created-at (plist-get props 'created-at))
                              (r-title (plist-get props 'title)))
                         (and
                          title r-title
                          (string= created-at r-created-at)
                          (string-prefix-p (replace-regexp-in-string "^#\\|[.][.][.]$" "" title)
                                           (replace-regexp-in-string " +" " " r-title)))))))))
      found)))

(defun consult-hn--fetch-page-async (input page async expected-search &optional buffer-callback)
  "Fetch a single page asynchronously.
INPUT is the search query string.
PAGE is the page number to fetch.
ASYNC is the callback function to send results downstream.
EXPECTED-SEARCH is the search term this request belongs to.
BUFFER-CALLBACK is an optional function called with the request buffer."
  (let* ((params (consult-hn--input->params input))
         (_ (setf (alist-get 'page params) (list page)))
         (search-type (if (and params
                               (thread-last
                                 params (alist-get 'tags)
                                 car-safe
                                 (funcall (lambda (x) (or x "")))
                                 (string-match-p "front_page")))
                          "search" "search_by_date"))
         (search-url (format "https://hn.algolia.com/api/v1/%s?%s"
                             search-type
                             (url-build-query-string params))))
    (let ((buffer (url-retrieve
                   search-url
                   (lambda (status)
                     ;; Only process if this is still the current search
                     (when (and (buffer-live-p (current-buffer))
                                (equal input expected-search))
                       (if-let ((error (plist-get status :error)))
                           (message "HN fetch error: %s" error)
                         (when (and url-http-end-of-headers
                                    (marker-position url-http-end-of-headers))
                           (goto-char url-http-end-of-headers)
                           (condition-case err
                               (let* ((json-object-type 'hash-table)
                                      (json-array-type 'list)
                                      (result (json-read))
                                      (rows (consult-hn--process-results result))
                                      (nbPages (gethash "nbPages" result))
                                      (current-page (gethash "page" result)))
                                 ;; Send results only if still current search
                                 (when (and rows (equal input expected-search))
                                   (funcall async rows))
                                 ;; Fetch next page if still current search
                                 (when (and (< (1+ current-page) nbPages)
                                            (equal input expected-search))
                                   (consult-hn--fetch-page-async
                                    input (1+ current-page) async expected-search buffer-callback)))
                             (json-end-of-file
                              (message "HN: JSON parse interrupted"))
                             (error
                              (message "HN parse error: %s" err)))))))
                   nil t)))
      (when (and buffer buffer-callback)
        (funcall buffer-callback buffer)))))

(defun consult-hn--async-source (async)
  "Async source function for HN search.
ASYNC is the callback function to send results downstream."
  (let ((request-buffers nil)
        (page 0)
        (nbPages nil)
        (current-search nil))  ; Track current search term
    (lambda (action)
      (pcase action
        ((pred stringp)
         ;; New search - update current search term
         (setq current-search action)
         (setq page 0 nbPages nil)

         ;; Cancel existing requests
         (dolist (buf request-buffers)
           (when (buffer-live-p buf)
             (let ((kill-buffer-query-functions nil))
               (kill-buffer buf))))
         (setq request-buffers nil)

         ;; Clear previous results
         (funcall async 'flush)

         ;; Start fetching if input is long enough
         (when (<= 2 (length action))
           (consult-hn--fetch-page-async
            action page async current-search
            (lambda (buf)
              (push buf request-buffers)))))

        ('destroy
         ;; Clean up all request buffers
         (dolist (buf request-buffers)
           (when (buffer-live-p buf)
             (let ((kill-buffer-query-functions nil))
               (kill-buffer buf))))
         (setq request-buffers nil))

        (_ (funcall async action))))))

(defun consult-hn--process-results (result)
  "Process the results from API response.
RESULT is the parsed JSON response from the HN API."
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
              (hn-base-url "https://news.ycombinator.com/item?id=%s")
              (hn-story-url (format hn-base-url (gethash "story_id" x)))
              (object-url (format hn-base-url (gethash "objectID" x)))
              (points (gethash "points" x))
              (num-comments (gethash "num_comments" x)))
         (propertize
          (replace-regexp-in-string " +" " " title)
          'title title
          'author author
          'comment comment-text
          'created-at created-at
          'story-url story-url
          'hn-story-url hn-story-url
          'hn-object-url object-url
          'ts ts
          'points points
          'num-comments num-comments))))))

(defun consult-hn (&optional initial)
  "Consult interface for searching on Hackernews.
INITIAL is for when it's called programmatically with an input."
  (interactive)
  (consult--read
   (consult--async-pipeline
    (consult--async-throttle)
    #'consult-hn--async-source
    (consult--async-transform #'consult-hn--async-transform))
   :lookup #'consult-hn--async-lookup
   :state (lambda (action cand)
            (when-let* ((hn-obj (consult-hn--plist-keywordize
                                 (text-properties-at 0 (or cand "")))))
              (pcase action
                ('preview (apply consult-hn-preview-fn hn-obj))
                ('return (apply consult-hn-browse-fn hn-obj)))))
   :prompt "HN Search: "
   :sort nil
   :initial (or initial consult-hn-initial-input-string)
   :history '(:input consult-hn--history)
   :require-match t
   :category 'consult-hn-result
   :annotate (lambda (x)
               ;; comments shown as annotation
               (if-let* ((comment (get-text-property 0 'comment x))
                         (ann-txt (replace-regexp-in-string
                                   "^" "  " ; prefix every line in the comment with an indent
                                   (consult-hn--fill-string comment 120 'full))))
                   (format "\n%s" ann-txt)
                 ""))))

(defun consult-hn--open (item)
  "Default Embark action for `consult-hn' ITEM."
  (thread-last
    (or item "")
    (text-properties-at 0)
    consult-hn--plist-keywordize
    (apply consult-hn-browse-fn)))

(when (featurep 'embark)
  (setf (alist-get 'consult-hn-result embark-default-action-overrides)
        #'consult-hn--open))

(provide 'consult-hn)
;;; consult-hn.el ends here
