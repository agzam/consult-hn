;;; consult-hn.el --- Hacker News search with Consult -*- lexical-binding: t; -*-
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
;;  This is an extension for https://github.com/minad/consult, for
;;  searching on Hacker News via its public API, finding stories and
;;  comments matching a query, date range, number of comments and
;;  points.
;;
;;; Code:

(require 'consult)
(require 'subr-x)
(require 'ts)
(require 'url)
(require 'url-http)
(require 'json)
(require 'dom)

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

(make-obsolete-variable
 'consult-hn-initial-input-string
 "pass a query and parameters to `consult-hn' as arguments instead."
 "1.1.0")

(defcustom consult-hn-max-comment-lines 2
  "Comment lines shown under a candidate.
Completion UIs size their display by candidate count and are blind to
the extra screen lines an annotation adds, so an uncapped comment
balloons the minibuffer (or posframe) far past its usual height.
Overflowing comments end in an ellipsis; the whole text stays
filterable and is shown in full when the item is opened."
  :type 'integer
  :group 'consult-hn)

(defcustom consult-hn-comment-width 120
  "Width the shown comment lines are filled to."
  :type 'integer
  :group 'consult-hn)

(defcustom consult-hn-hits-per-page 100
  "Hits asked for per API page.
The endpoint answers a large page as readily as a small one, so the
same coverage costs fewer round trips: a broad query needs 50 requests
at 20 hits a page and 10 at 100.  Lower it on a slow link."
  :type 'integer
  :group 'consult-hn)

(defcustom consult-hn-max-pages 10
  "Pages fetched for one search before the chain stops.
Pagination otherwise runs to whatever page count the endpoint reports,
which for a broad query is dozens of requests for candidates nobody
scrolls to."
  :type 'integer
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

(defvar consult-hn--params
  '(:query nil :type all :author nil :points nil :comments nil
    :range all :front-page nil :url-match nil :sort nil)
  "Search parameters, the single source of truth for a request.
Keys: `:query' string, `:type' one of `all', `story', `comment',
`:author' string, `:points' and `:comments' integers, `:range' one of
`all', `24h', `week', `month', `year', `:front-page' and `:url-match'
booleans, `:sort' nil, `date' or `relevance'.  It outlives a session,
so the next one starts where the last left off.")

(defvar consult-hn--generation 0
  "Counter identifying the search a request belongs to.
Bumped by the source whenever it restarts or is torn down, so a
response that outlives its request can tell that nobody wants it.")

(defvar consult-hn--seen (make-hash-table :test 'equal)
  "Item ids already delivered for the current search.")

(defvar consult-hn--restart nil
  "Runs the live session's search again, nil when no session is open.
Input reaches the pipeline only when the minibuffer text changes, and
the throttle drops input equal to the one before it, so a parameter
change over unchanged text has no way in.  The source therefore hands
out its own way back in, which is what the parameter commands call.")

(defconst consult-hn--range-seconds
  '((24h . 86400) (week . 604800) (month . 2592000) (year . 31536000))
  "How far back each `:range' value reaches, in seconds.")

(defun consult-hn--nonblank (str)
  "STR unless it is nil or nothing but whitespace."
  (unless (or (null str) (string-blank-p str)) str))

(defun consult-hn--params-tags (params)
  "Value of the API `tags' parameter for PARAMS, or nil.
Comma-joined tags are an AND on the endpoint."
  (when-let* ((tags (delq nil
                          (list (pcase (plist-get params :type)
                                  ('story "story")
                                  ('comment "comment"))
                                (when-let* ((author (consult-hn--nonblank
                                                     (plist-get params :author))))
                                  (concat "author_" author))
                                (when (plist-get params :front-page)
                                  "front_page")))))
    (string-join tags ",")))

(defun consult-hn--params-numeric-filters (params)
  "Value of the API `numericFilters' parameter for PARAMS, or nil.
The endpoint takes several conditions in one comma-separated value."
  (when-let* ((filters
               (delq nil
                     (list (when-let* ((points (plist-get params :points)))
                             (format "points>%s" points))
                           (when-let* ((comments (plist-get params :comments)))
                             (format "num_comments>%s" comments))
                           (when-let* ((secs (alist-get
                                              (plist-get params :range)
                                              consult-hn--range-seconds)))
                             (format "created_at_i>%d"
                                     (- (time-convert nil 'integer) secs)))))))
    (string-join filters ",")))

(defun consult-hn--params-searchable-p (params)
  "Whether PARAMS ask the endpoint for something without any query at all.
An author, a kind of item or a threshold is a search in itself; saying
where the query should match is not."
  (and (or (consult-hn--params-tags params)
           (consult-hn--params-numeric-filters params))
       t))

(defun consult-hn--params-render (params &optional page)
  "API parameter alist for PARAMS, asking for PAGE when given.
Values go in raw: `url-build-query-string' hexifies on the way out, so
encoding here would escape the escapes.  What PARAMS sets wins over
`consult-hn-default-search-params', which in turn wins over the page
size this package would otherwise ask for."
  (let ((rendered
         (delq nil
               (list (when-let* ((query (consult-hn--nonblank
                                         (plist-get params :query))))
                       (list 'query query))
                     (when-let* ((tags (consult-hn--params-tags params)))
                       (list 'tags tags))
                     (when-let* ((numeric (consult-hn--params-numeric-filters
                                           params)))
                       (list 'numericFilters numeric))
                     (when (plist-get params :url-match)
                       (list 'restrictSearchableAttributes "url"))))))
    (dolist (default consult-hn-default-search-params)
      (when (and (memq (car default) consult-hn--api-allowed-keys)
                 (not (assq (car default) rendered)))
        (setq rendered (append rendered (list default)))))
    (unless (assq 'hitsPerPage rendered)
      (setq rendered (append rendered
                             (list (list 'hitsPerPage
                                         consult-hn-hits-per-page)))))
    (if page
        (append rendered (list (list 'page page)))
      rendered)))

(defun consult-hn--params-endpoint (params &optional rendered)
  "Endpoint PARAMS ask for, given their RENDERED parameter alist.
An unset `:sort' reproduces the rule this package has always used,
where asking for the front page means asking for its ranking."
  (pcase (plist-get params :sort)
    ('relevance "search")
    ('date "search_by_date")
    (_ (if (string-match-p
            "front_page"
            (or (car (alist-get 'tags (or rendered
                                          (consult-hn--params-render params))))
                ""))
           "search"
         "search_by_date"))))

(defun consult-hn--params-chips (params)
  "PARAMS as a compact decoration for the prompt, empty when they are not set.
The only place that knows the chip vocabulary.  A state that constrains
nothing renders nothing at all, so the prompt of someone who never
touches a parameter is the prompt this package has always shown."
  (let ((chips
         (delq nil
               (list (pcase (plist-get params :type)
                       ('story "story")
                       ('comment "comment"))
                     (consult-hn--nonblank (plist-get params :author))
                     (when-let* ((points (plist-get params :points)))
                       (format ">%sp" points))
                     (when-let* ((comments (plist-get params :comments)))
                       (format ">%sc" comments))
                     (pcase (plist-get params :range)
                       ('24h "24h")
                       ('week "7d")
                       ('month "30d")
                       ('year "1y"))
                     (when (plist-get params :front-page) "front")
                     (when (plist-get params :url-match) "url")
                     (pcase (plist-get params :sort)
                       ('relevance "rel")
                       ('date "date"))))))
    (if chips
        (propertize (format " [%s]" (string-join chips " · "))
                    'face 'consult-narrow-indicator)
      "")))

(defun consult-hn--api-url (params rendered)
  "Request URL for PARAMS carrying the RENDERED parameter alist."
  (format "https://hn.algolia.com/api/v1/%s?%s"
          (consult-hn--params-endpoint params rendered)
          (url-build-query-string rendered)))

(cl-defun consult-hn-eww (&key story-url title hn-story-url author created-at hn-object-url num-comments points comment &allow-other-keys)
  "Open hacker News item in eww buffer.
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
    (eww (or hn-story-url story-url) :new-buffer)))

(defun consult-hn--fill-string (str &optional width justify)
  "Fills the STR string with WIDTH and JUSTIFY options."
  (let ((fill-column (or width 50))
        (use-hard-newlines t))
    (with-temp-buffer
      (insert str)
      (fill-region-as-paragraph (point-min) (point-max) (or justify 'left))
      (buffer-string))))

(defun consult-hn--comment-annotation (comment)
  "Render COMMENT as an indented block of at most a few lines.
Only as much text as can possibly be shown is filled, so the cost does
not grow with the length of the comment.  Returns nil for no comment."
  (unless (or (null comment) (string-blank-p comment))
    (let* ((cap (max 1 consult-hn-max-comment-lines))
           (width consult-hn-comment-width)
           (budget (* (1+ cap) width))
           (bounded (truncate-string-to-width comment budget))
           (lines (split-string
                   (consult-hn--fill-string bounded width 'full) "\n" t))
           (clipped (or (< cap (length lines))
                        (< (length bounded) (length comment))))
           (kept (take cap lines))
           (kept (if clipped
                     (append (butlast kept)
                             (list (concat (string-trim-right (car (last kept)))
                                           "…")))
                   kept)))
      (mapconcat (lambda (l) (concat "  " l)) kept "\n"))))

(defun consult-hn--annotate (cand)
  "Annotation for CAND, computed when the candidate was built."
  (or (get-text-property 0 'consult-hn--annotation cand) ""))

(defvar vertico-count)

(defun consult-hn--scale-vertico-count ()
  "Shrink the session's `vertico-count' to the usual height budget.
Vertico sizes its display by candidate count, blind to the screen
lines annotations add, and every candidate here occupies its own line
plus up to `consult-hn-max-comment-lines' comment lines.  Dividing the
count by that factor keeps the session around the height a plain
vertico session occupies.  A count already buffer-local, e.g. set
through vertico-multiform, is respected."
  (when (and (boundp 'vertico-count)
             (not (local-variable-p 'vertico-count)))
    (setq-local vertico-count
                (max 4 (floor vertico-count
                              (1+ (max 1 consult-hn-max-comment-lines)))))))

(defvar consult-hn--chips-overlay nil
  "Prompt decoration carrying the live session's parameters.")

(defun consult-hn--chips-install ()
  "Decorate the prompt of the session being set up with its parameters.
The position is the one `consult-narrow' decorates, so the two stack
rather than fight if narrowing is ever enabled here."
  (consult-hn--chips-remove)
  (setq consult-hn--chips-overlay
        (consult--make-overlay
         (1- (minibuffer-prompt-end)) (minibuffer-prompt-end)
         'category 'consult-hn-chips-overlay
         'before-string (consult-hn--params-chips consult-hn--params)))
  (add-hook 'minibuffer-exit-hook #'consult-hn--chips-remove nil t))

(defun consult-hn--chips-update ()
  "Show what the parameters say now, if a session is there to show it on."
  (when consult-hn--chips-overlay
    (overlay-put consult-hn--chips-overlay 'before-string
                 (consult-hn--params-chips consult-hn--params))))

(defun consult-hn--chips-remove ()
  "Take the chips off, however the session they belong to ended.
Minibuffers are reused rather than killed, so an overlay outliving its
session is an overlay in somebody else's prompt."
  (when consult-hn--chips-overlay
    (delete-overlay consult-hn--chips-overlay)
    (setq consult-hn--chips-overlay nil)))

(defun consult-hn--session-setup ()
  "Prepare the minibuffer the session is about to run in."
  (consult-hn--scale-vertico-count)
  (consult-hn--chips-install))

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

(defun consult-hn--input-split (input)
  "INPUT split into its query part and its legacy parameter part."
  (split-string input "--" nil " +"))

(defun consult-hn--legacy-pairs (input)
  "Parameters from the legacy ` -- key=value' suffix of INPUT.
Undocumented and unwarned, still parsed: the package is published and
someone may have the syntax in a keybinding."
  (when-let* ((parts (cadr (consult-hn--input-split input))))
    (cl-loop for pair in (split-string parts " +" t)
             when (string-match "\\([^=]+\\)=\\(.+\\)" pair)
             collect (list (intern (match-string 1 pair))
                           (match-string 2 pair)))))

(defun consult-hn--request-url (input page)
  "Request URL for INPUT at PAGE under the current parameter state.
INPUT carries the query; a legacy ` -- key=value' suffix is layered
over the rendered state, so what a caller spelled out by hand wins."
  (let* ((params (plist-put (copy-sequence consult-hn--params)
                            :query (car-safe (consult-hn--input-split input))))
         (legacy (seq-filter (lambda (x)
                               (memq (car x) consult-hn--api-allowed-keys))
                             (consult-hn--legacy-pairs input)))
         (rendered (append (seq-remove (lambda (x) (assq (car x) legacy))
                                       (consult-hn--params-render params page))
                           legacy)))
    (consult-hn--api-url params rendered)))

(defun consult-hn--parse-row-for-lookup (cand-str)
  "Parse displayed candidate string CAND-STR and break into parts."
  (let* ((pattern (rx (group-n 1 (+? not-newline))         ; title (non-greedy match)
                      (>= 3 space)                         ; separator
                      (group-n 2 (+ (not space)))          ; author
                      (>= 3 space)                         ; separator
                      (group-n 3 (+? not-newline))         ; ago
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
              (raw-comment (get-text-property 0 'comment x))
              (comment (if raw-comment (propertize raw-comment 'invisible t) ""))
              ;; rendered once here rather than per redisplay, which is
              ;; what `:annotate' would otherwise cost for every
              ;; visible candidate on every keystroke
              (annotation (consult-hn--comment-annotation raw-comment)))
         (propertize (format "%-75s   %-20s   %20s   %s   %s"
                             row author ago created-at comment)
                     'consult-hn--annotation
                     (and annotation (concat "\n" annotation))))))))

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

(defvar url-http-end-of-headers) ; used by url-http

(defun consult-hn--dedup (rows)
  "ROWS that this search has not delivered already.
Sorted by date the endpoint paginates over a moving window, so an item
can sit on two pages when newer ones arrive between the requests."
  (seq-filter (lambda (row)
                (let ((id (get-text-property 0 'object-id row)))
                  (cond ((null id) t)
                        ((gethash id consult-hn--seen) nil)
                        (t (puthash id t consult-hn--seen) t))))
              rows))

(defun consult-hn--fetch-page-async (input page async generation &optional buffer-callback)
  "Fetch a single page asynchronously.
INPUT is the search query string.
PAGE is the page number to fetch.
ASYNC is the callback function to send results downstream.
GENERATION is the search this request belongs to.  Killing the request
buffer only cancels what has not been delivered yet; a response already
on its way, or one arriving in a buffer the caller never saw, still
runs its callback, and the generation is what tells it to stop.
BUFFER-CALLBACK is an optional function called with the request buffer."
  (let ((search-url (consult-hn--request-url input page)))
    (let ((buffer (url-retrieve
                   search-url
                   (lambda (status)
                     ;; Only process if this is still the current search
                     (when (and (buffer-live-p (current-buffer))
                                (eql generation consult-hn--generation))
                       (if-let* ((error (plist-get status :error)))
                           (message "HN fetch error: %s" error)
                         ;; When `url-retrieve` fetches an HTTP resource, it:
                         ;; 1. Creates a buffer with the full HTTP response (headers + body)
                         ;; 2. Sets `url-http-end-of-headers` as a marker pointing to the position right
                         ;; after the HTTP headers end (typically after the blank line that separates
                         ;; headers from body)
                         (when (and url-http-end-of-headers
                                    (marker-position url-http-end-of-headers))
                           (goto-char url-http-end-of-headers)
                           (condition-case err
                               (let* ((json-object-type 'hash-table)
                                      (json-array-type 'list)
                                      (result (json-read))
                                      (rows (consult-hn--dedup
                                             (consult-hn--process-results result)))
                                      (nb-pages (gethash "nbPages" result))
                                      (current-page (gethash "page" result))
                                      (next-page (1+ current-page)))
                                 (when rows
                                   (funcall async rows))
                                 ;; the endpoint reports dozens of pages
                                 ;; for a broad query; the tail of that
                                 ;; is candidates nobody scrolls to
                                 (when (and (< next-page nb-pages)
                                            (< next-page consult-hn-max-pages))
                                   (consult-hn--fetch-page-async
                                    input next-page async generation buffer-callback)))
                             ;; a killed request buffer truncates the
                             ;; response mid-parse, which is what
                             ;; cancelling a search looks like from here
                             (json-end-of-file nil)
                             (error
                              (message "HN parse error: %S" err)))))))
                   nil t)))
      (when (and buffer buffer-callback)
        (funcall buffer-callback buffer)))))

(defun consult-hn--async-source (async)
  "Async source function for HN search.
ASYNC is the callback function to send results downstream."
  (let ((request-buffers nil)
        ;; a string from the start: a parameter command can reach the
        ;; restart before the first input reaches the source
        (input ""))
    (cl-labels ((cancel ()
                  ;; a newer generation retires whatever is still in the
                  ;; air; killing the buffers stops the rest from arriving
                  (setq consult-hn--generation (1+ consult-hn--generation))
                  (clrhash consult-hn--seen)
                  (dolist (buf request-buffers)
                    (when (buffer-live-p buf)
                      (let ((kill-buffer-query-functions nil))
                        (kill-buffer buf))))
                  (setq request-buffers nil))
                (restart ()
                  (cancel)
                  ;; the previous result set goes at once, rather than
                  ;; the new one arriving mixed into it
                  (funcall async 'flush)
                  ;; parameters can carry a search on their own: an
                  ;; author, or the front page, needs no query
                  (when (or (<= 2 (length input))
                            (consult-hn--params-searchable-p consult-hn--params))
                    (consult-hn--fetch-page-async
                     input 0 async consult-hn--generation
                     (lambda (buf)
                       (push buf request-buffers))))))
      (lambda (action)
        (pcase action
          ((pred stringp)
           (setq input action)
           (restart))

          ('setup
           (setq consult-hn--restart #'restart)
           (funcall async action))

          ('destroy
           (setq consult-hn--restart nil)
           (cancel)
           ;; the stages downstream tear down here too: consult's own
           ;; indicator deletes its overlay and the refresh stage its
           ;; timer, neither of which happens if this is swallowed
           (funcall async action))

          (_ (funcall async action)))))))

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
                                ;; `dom-inner-text' is the sanctioned
                                ;; replacement but arrived in 31.1 and
                                ;; joins nodes without a separator,
                                ;; running words together
                                (with-suppressed-warnings ((obsolete dom-texts))
                                  (dom-texts (libxml-parse-html-region))))))
              (title (or (gethash "title" x)
                         (gethash "story_title" x)
                         ""))
              (story-url (or (gethash "story_url" x)
                             (gethash "url" x)))
              (created-at (gethash "created_at" x))
              (ts (gethash "created_at_i" x))
              (hn-base-url "https://news.ycombinator.com/item?id=%s")
              (hn-story-url (format hn-base-url (gethash "story_id" x)))
              (object-id (gethash "objectID" x))
              (object-url (format hn-base-url object-id))
              (points (gethash "points" x))
              (num-comments (gethash "num_comments" x)))
         (when title
           (propertize
            (replace-regexp-in-string " +" " " title)
            'title title
            'object-id object-id
            'author author
            'comment comment-text
            'created-at created-at
            'story-url story-url
            'hn-story-url hn-story-url
            'hn-object-url object-url
            'ts ts
            'points points
            'num-comments num-comments)))))
    (seq-filter #'identity)))

(defun consult-hn--param-set (key value)
  "Give KEY the VALUE for the live session and search on it at once.
The session keeps its input, its history and its window; only the
result set is replaced."
  (setq consult-hn--params
        (plist-put (copy-sequence consult-hn--params) key value))
  (consult-hn--chips-update)
  (when consult-hn--restart
    (funcall consult-hn--restart)))

(defun consult-hn--cycle (value choices)
  "The entry after VALUE in CHOICES, wrapping around at the end."
  (car (or (cdr (memq value choices)) choices)))

(defun consult-hn--read (prompt &optional initial)
  "Read a string with PROMPT and INITIAL from inside the session.
Reading while a minibuffer is live needs recursive minibuffers allowed,
which is bound here and only here: setting it globally changes how every
other package behaves."
  (consult--local-let ((enable-recursive-minibuffers t))
    (string-trim (read-string prompt initial))))

(defun consult-hn--read-threshold (prompt current)
  "Read a threshold with PROMPT, offering CURRENT to edit.
An empty answer means no threshold at all."
  (let ((answer (consult-hn--read prompt (and current
                                              (number-to-string current)))))
    (cond ((string-empty-p answer) nil)
          ((string-match-p "\\`[0-9]+\\'" answer) (string-to-number answer))
          (t (user-error "Not a whole number: %s" answer)))))

(defun consult-hn-session-type ()
  "Cycle the live session between everything, stories and comments."
  (interactive)
  (consult--require-minibuffer)
  (consult-hn--param-set
   :type (consult-hn--cycle (plist-get consult-hn--params :type)
                            '(all story comment))))

(defun consult-hn-session-range ()
  "Cycle how far back the live session reaches."
  (interactive)
  (consult--require-minibuffer)
  (consult-hn--param-set
   :range (consult-hn--cycle (plist-get consult-hn--params :range)
                             '(all 24h week month year))))

(defun consult-hn-session-sort ()
  "Cycle the order the live session asks the endpoint for.
The first choice is the one this package has always made on its own:
newest first, unless the front page is asked for."
  (interactive)
  (consult--require-minibuffer)
  (consult-hn--param-set
   :sort (consult-hn--cycle (plist-get consult-hn--params :sort)
                            '(nil date relevance))))

(defun consult-hn-session-front-page ()
  "Toggle whether the live session is confined to the front page."
  (interactive)
  (consult--require-minibuffer)
  (consult-hn--param-set :front-page
                         (not (plist-get consult-hn--params :front-page))))

(defun consult-hn-session-url-match ()
  "Toggle whether the live session matches URLs rather than text."
  (interactive)
  (consult--require-minibuffer)
  (consult-hn--param-set :url-match
                         (not (plist-get consult-hn--params :url-match))))

(defun consult-hn-session-author ()
  "Confine the live session to one author, or release it again."
  (interactive)
  (consult--require-minibuffer)
  (consult-hn--param-set
   :author (consult-hn--nonblank
            (consult-hn--read "Author (empty for anyone): "
                              (plist-get consult-hn--params :author)))))

(defun consult-hn-session-points ()
  "Set the points the live session's items have to beat."
  (interactive)
  (consult--require-minibuffer)
  (consult-hn--param-set
   :points (consult-hn--read-threshold "Minimum points (empty for none): "
                                       (plist-get consult-hn--params :points))))

(defun consult-hn-session-comments ()
  "Set the comment count the live session's items have to beat."
  (interactive)
  (consult--require-minibuffer)
  (consult-hn--param-set
   :comments (consult-hn--read-threshold
              "Minimum comments (empty for none): "
              (plist-get consult-hn--params :comments))))

(defvar consult-hn-session-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c t") #'consult-hn-session-type)
    (define-key map (kbd "C-c a") #'consult-hn-session-author)
    (define-key map (kbd "C-c p") #'consult-hn-session-points)
    (define-key map (kbd "C-c c") #'consult-hn-session-comments)
    (define-key map (kbd "C-c r") #'consult-hn-session-range)
    (define-key map (kbd "C-c f") #'consult-hn-session-front-page)
    (define-key map (kbd "C-c u") #'consult-hn-session-url-match)
    (define-key map (kbd "C-c s") #'consult-hn-session-sort)
    map)
  "Parameter commands, live for as long as a session is.
One prefix, one command per parameter, so `which-key' and friends
document the surface without this package saying anything.  Not
narrowing keys: narrowing holds one value at a time and is unbound for
most users anyway.")

(defconst consult-hn--param-keys
  '(:query :type :author :points :comments :range :front-page :url-match :sort)
  "Parameters a caller may set, the parameter model and nothing else.")

(defun consult-hn--params-merge (params overrides)
  "PARAMS with the OVERRIDES plist layered over them.
A keyword outside the model is refused rather than ignored: silently
dropping it leaves a keybinding that searches for something else than
it says."
  (let ((merged (copy-sequence params)))
    (cl-loop for (key value) on overrides by #'cddr
             do (unless (memq key consult-hn--param-keys)
                  (user-error "Unknown `consult-hn' parameter: %S" key))
             (setq merged (plist-put merged key value)))
    merged))

(defun consult-hn (&optional query &rest params)
  "Consult interface for searching on Hacker News.
QUERY is what the session starts with, for calling this from Lisp or
from a keybinding.  PARAMS are keywords from the parameter model,
`:type' `:author' `:points' `:comments' `:range' `:front-page'
`:url-match' `:sort', which hold for this call only and do not outlive
it.  Called interactively it takes neither, and the session inherits
whatever the last one was shaped into."
  (interactive)
  (let* ((consult-hn--params (consult-hn--params-merge consult-hn--params params))
         (initial (or query (plist-get consult-hn--params :query)
                      (with-suppressed-warnings
                          ((obsolete consult-hn-initial-input-string))
                        consult-hn-initial-input-string))))
    (minibuffer-with-setup-hook #'consult-hn--session-setup
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
       :keymap consult-hn-session-map
       :sort nil
       :initial initial
       :history '(:input consult-hn--history)
       :require-match t
       :category 'consult-hn-result
       :annotate #'consult-hn--annotate))))

(defun consult-hn--open (item)
  "Default Embark action for `consult-hn' ITEM."
  (thread-last
    (or item "")
    (text-properties-at 0)
    consult-hn--plist-keywordize
    (apply consult-hn-browse-fn)))

(defvar embark-default-action-overrides)
(when (featurep 'embark)
  (setf (alist-get 'consult-hn-result embark-default-action-overrides)
        #'consult-hn--open))

(provide 'consult-hn)
;;; consult-hn.el ends here
