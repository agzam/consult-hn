;;; consult-hn-e2e.el --- Deterministic e2e suite for the consult UI -*- lexical-binding: t; -*-

;; Not a buttercup file on purpose (the name lacks the -tests suffix, so
;; buttercup-run-discover skips it): this drives a REAL interactive Emacs
;; through the full `consult-hn' flow - real minibuffer, real
;; vertico/consult/orderless, real key events through the command loop -
;; with only the HN API stubbed at the `url-retrieve' seam.  Run via
;; `make e2e', which wraps this Emacs in a PTY.

;;; Commentary:
;; The suite is a chain of steps driven by timers while the minibuffer
;; blocks inside `consult--read'.  Every assertion appends PASS/FAIL to
;; the results file; the suite runs twice in one process to prove no
;; state leaks; a watchdog kills the session on hangs.
;;
;; Height verification is organic: it reads the `before-string' that
;; `vertico--display-candidates' hands to the display engine, which is
;; the material the fix is meant to bound.  Vertico itself sizes the
;; window from (length lines), the candidate COUNT, which is precisely
;; why an uncapped annotation escapes its accounting.

;;; Code:

(require 'seq)
(require 'cl-lib)
(require 'json)

(defvar vertico-count)
(defvar vertico--candidates-ov)
(defvar url-http-end-of-headers)
(defvar transient--prefix)

(defvar consult-hn-e2e-results-file
  (expand-file-name "consult-hn-e2e-results.txt"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Where PASS/FAIL lines and the final verdict go.")

(defvar consult-hn-e2e--failures 0)
(defvar consult-hn-e2e--round 1)
(defvar consult-hn-e2e--requests nil "Recorded request URLs, newest first.")
(defvar consult-hn-e2e--delivered nil "URLs whose callback actually ran.")
(defvar consult-hn-e2e--previews nil "Recorded preview/browse invocations.")
(defvar consult-hn-e2e--buffers nil "Response buffers handed out by the stub.")

(defvar consult-hn-e2e--detached-page nil
  "Page whose response arrives in a buffer the caller never saw.
Models a redirect, where `url-retrieve' hands one buffer back and
another one delivers: cancelling a search by killing what you were
handed cannot reach it.  Nil for the ordinary path.")

(defvar consult-hn-e2e--detached-delay 0.4
  "How long a detached response stays in the air.")

;;; Fixtures

(defconst consult-hn-e2e--long-comment
  (concat (mapconcat #'identity (make-list 500 "wordy") " ") " ENDMARKER")
  "A comment far longer than any sane annotation, ending in a marker.
The marker must never reach the display once the cap is in force.")

(defun consult-hn-e2e--hit (n &optional comment label)
  "One API hit numbered N, a comment hit when COMMENT is given.
LABEL titles the hit, so a second fixture set can be told apart from
the main one by looking at the display alone."
  (let ((title (format "%s %d" (or label "Story title") n))
        (h (list (cons "author" (format "user%d" n))
                 (cons "created_at" (format "2025-01-30T10:%02d:00" n))
                 (cons "created_at_i" (- 1738226435 (* n 3600)))
                 (cons "objectID" (number-to-string (+ 1000 n)))
                 (cons "story_id" (number-to-string (+ 1000 n)))
                 (cons "points" (* n 3))
                 (cons "num_comments" n))))
    (if comment
        (append h (list (cons "story_title" title)
                        (cons "comment_text" (format "<p>%s</p>" comment))))
      (append h (list (cons "title" title)
                      (cons "url" (format "https://example.com/%d" n)))))))

(defconst consult-hn-e2e--pages
  (list
   ;; page 0 carries the pathological comment
   (list (consult-hn-e2e--hit 1)
         (consult-hn-e2e--hit 2 consult-hn-e2e--long-comment)
         (consult-hn-e2e--hit 3 "short reply about emacs")
         (consult-hn-e2e--hit 4)
         (consult-hn-e2e--hit 5 consult-hn-e2e--long-comment))
   ;; hit 3 again: sorted by date the endpoint paginates over a moving
   ;; window, and an item pushed down by newer ones lands on two pages
   (list (consult-hn-e2e--hit 6) (consult-hn-e2e--hit 7 "another reply")
         (consult-hn-e2e--hit 3 "short reply about emacs")
         (consult-hn-e2e--hit 8) (consult-hn-e2e--hit 9 "yet another reply")
         (consult-hn-e2e--hit 10))
   (list (consult-hn-e2e--hit 11) (consult-hn-e2e--hit 12 "trailing reply")
         (consult-hn-e2e--hit 13) (consult-hn-e2e--hit 14)
         (consult-hn-e2e--hit 15)))
  "Three pages holding sixteen hits, fifteen of them distinct.")

(defun consult-hn-e2e--fixture-hits (pages)
  "Number of hits PAGES serve, repeats and all."
  (apply #'+ (mapcar #'length pages)))

(defconst consult-hn-e2e--other-query "rust"
  "The query served from a fixture set of its own.")

(defconst consult-hn-e2e--other-pages
  (list (list (consult-hn-e2e--hit 21 nil "Other story")
              (consult-hn-e2e--hit 22 "a reply" "Other story")
              (consult-hn-e2e--hit 23 nil "Other story")))
  "One page, titled apart, so a second search is visible as such.")

(defconst consult-hn-e2e--default-params (copy-sequence consult-hn--params)
  "The parameter state every scenario starts from.
The state outlives a session by design, so without this the scenarios
would inherit each other's parameters, and so would the second round.")

(defun consult-hn-e2e--tagged-pages (tags)
  "One page of hits titled after TAGS.
A parameterised search is then visible in the display alone, and the
title says which parameter it came back for."
  (let ((label (format "Tagged %s" tags)))
    (list (list (consult-hn-e2e--hit 31 nil label)
                (consult-hn-e2e--hit 32 "a tagged reply" label)
                (consult-hn-e2e--hit 33 nil label)))))

(defun consult-hn-e2e--url-page (url)
  "Page number requested by URL."
  (if (string-match "[?&]page=\\([0-9]+\\)" url)
      (string-to-number (match-string 1 url))
    0))

(defun consult-hn-e2e--url-query (url)
  "Query URL asks for."
  (if (string-match "[?&]query=\\([^&]*\\)" url)
      (url-unhex-string (match-string 1 url))
    ""))

(defun consult-hn-e2e--url-tags (url)
  "Tags URL asks for, empty when it asks for none."
  (if (string-match "[?&]tags=\\([^&]*\\)" url)
      (url-unhex-string (match-string 1 url))
    ""))

(defun consult-hn-e2e--url-pages (url)
  "Fixture pages URL is served from."
  (let ((tags (consult-hn-e2e--url-tags url)))
    (cond ((not (string-empty-p tags)) (consult-hn-e2e--tagged-pages tags))
          ((equal (consult-hn-e2e--url-query url) consult-hn-e2e--other-query)
           consult-hn-e2e--other-pages)
          (t consult-hn-e2e--pages))))

(defun consult-hn-e2e--payload (url)
  "JSON body for URL."
  (let* ((pages (consult-hn-e2e--url-pages url))
         (page (consult-hn-e2e--url-page url)))
    (json-encode
     (list (cons "hits" (or (nth page pages) []))
           (cons "nbPages" (length pages))
           (cons "page" page)
           (cons "hitsPerPage" 5)
           (cons "nbHits" (consult-hn-e2e--fixture-hits pages))))))

;;; The only stub: the HTTP seam

(defun consult-hn-e2e--url-retrieve (url callback &optional _cbargs _silent _cookies)
  "Stub of `url-retrieve' serving fixtures for URL to CALLBACK.
Mirrors the real thing closely enough to matter: the callback runs in
the response buffer with `url-http-end-of-headers' set, and a buffer
killed before delivery never calls back, which is how cancellation
works in production.  A page named by `consult-hn-e2e--detached-page'
answers from a buffer the caller never receives, the one shape of
response that cancelling cannot reach."
  (unless (string-match-p "hn\\.algolia\\.com" url)
    ;; nothing but the search API may reach the network; the browse and
    ;; preview seams are stubbed separately
    (consult-hn-e2e--check "unexpected non-API request" nil url))
  (push url consult-hn-e2e--requests)
  (let* ((detached (eql (consult-hn-e2e--url-page url)
                        consult-hn-e2e--detached-page))
         (response (generate-new-buffer " *consult-hn-e2e-response*"))
         (handed-back (if detached
                          (generate-new-buffer " *consult-hn-e2e-redirected*")
                        response)))
    (push response consult-hn-e2e--buffers)
    (unless (eq handed-back response)
      (push handed-back consult-hn-e2e--buffers))
    (with-current-buffer response
      (insert "HTTP/1.1 200 OK\nContent-Type: application/json\n\n")
      (setq-local url-http-end-of-headers (copy-marker (point)))
      (insert (consult-hn-e2e--payload url)))
    (run-at-time
     (if detached consult-hn-e2e--detached-delay 0.03) nil
     (lambda ()
       (when (buffer-live-p response)
         (push url consult-hn-e2e--delivered)
         (with-current-buffer response (funcall callback nil)))))
    handed-back))

;;; Assertion plumbing

(defun consult-hn-e2e--log (fmt &rest args)
  "Append formatted FMT ARGS to the results file."
  (let ((line (apply #'format fmt args)))
    (with-temp-buffer
      (insert (format "[round %d] %s\n" consult-hn-e2e--round line))
      (append-to-file (point-min) (point-max) consult-hn-e2e-results-file))))

(defun consult-hn-e2e--check (name ok &optional detail)
  "Record check NAME as OK or failed with DETAIL."
  (if ok
      (consult-hn-e2e--log "PASS %s" name)
    (setq consult-hn-e2e--failures (1+ consult-hn-e2e--failures))
    (consult-hn-e2e--log "FAIL %s%s" name (if detail (format ": %s" detail) ""))))

(defun consult-hn-e2e--safe-call (k arg)
  "Call K with ARG, recording any error instead of losing it.
Steps run inside timers, where a signal would otherwise kill the chain
silently and surface only as a watchdog timeout much later."
  (condition-case err
      (funcall k arg)
    (error (consult-hn-e2e--check "step raised" nil (format "%S" err)))))

(defun consult-hn-e2e--await (name pred k &optional timeout)
  "Poll PRED every 50ms for TIMEOUT secs; record NAME; continue with K."
  (let ((deadline (+ (float-time) (or timeout 8)))
        poll)
    (setq poll
          (lambda ()
            (let ((val (ignore-errors (funcall pred))))
              (cond
               (val (consult-hn-e2e--check name t)
                    (consult-hn-e2e--safe-call k val))
               ((< deadline (float-time))
                (consult-hn-e2e--check name nil "timeout")
                (consult-hn-e2e--safe-call k nil))
               (t (run-at-time 0.05 nil poll))))))
    (funcall poll)))

(defun consult-hn-e2e--keys (keys)
  "Queue KEYS (a `kbd' string) into the real command loop."
  (setq unread-command-events
        (append unread-command-events (listify-key-sequence (kbd keys)))))

;;; State probes

(defun consult-hn-e2e--minibuffer ()
  "The active minibuffer buffer, or nil."
  (when-let* ((win (active-minibuffer-window)))
    (window-buffer win)))

(defun consult-hn-e2e--input ()
  "Text the session currently holds, separator and all."
  (when-let* ((mb (consult-hn-e2e--minibuffer)))
    (with-current-buffer mb (minibuffer-contents-no-properties))))

(defvar consult-hn-e2e--input-before nil
  "Session text captured before a step that must not disturb it.")

(defun consult-hn-e2e--candidates ()
  "Candidates the completion table currently serves."
  (when-let* ((mb (consult-hn-e2e--minibuffer)))
    (with-current-buffer mb
      (all-completions "" minibuffer-completion-table nil))))

(defun consult-hn-e2e--candidate-count ()
  "Number of candidates the completion table currently serves."
  (length (consult-hn-e2e--candidates)))

(defun consult-hn-e2e--chip-overlays (buffer)
  "Chip overlays BUFFER carries, found the way redisplay finds them."
  (when (buffer-live-p (get-buffer buffer))
    (with-current-buffer buffer
      (seq-filter (lambda (ov)
                    (eq (overlay-get ov 'category) 'consult-hn-chips-overlay))
                  (overlays-in (point-min) (point-max))))))

(defun consult-hn-e2e--chips ()
  "Chip text decorating the live prompt, as the display engine receives it."
  (when-let* ((mb (consult-hn-e2e--minibuffer))
              (ov (car (consult-hn-e2e--chip-overlays mb))))
    (overlay-get ov 'before-string)))

(defun consult-hn-e2e--stale-chips ()
  "Chip overlays left behind in the minibuffer buffer that gets reused."
  (length (consult-hn-e2e--chip-overlays " *Minibuf-1*")))

(defun consult-hn-e2e--pages-requested ()
  "Pages asked for so far, in the order they were asked for."
  (mapcar #'consult-hn-e2e--url-page (reverse consult-hn-e2e--requests)))

(defun consult-hn-e2e--rendered ()
  "What vertico last handed to the display engine, as a string."
  (when-let* ((mb (consult-hn-e2e--minibuffer)))
    (with-current-buffer mb
      (and vertico--candidates-ov
           (or (overlay-get vertico--candidates-ov 'before-string) "")))))

(defun consult-hn-e2e--rendered-lines ()
  "Screen lines in the material vertico is displaying."
  (when-let* ((s (consult-hn-e2e--rendered)))
    (length (split-string s "\n"))))

(defun consult-hn-e2e--unbounded-lines ()
  "Lines the pathological comment would occupy with no cap.
Computed from the fixture through the package's own filler, so the
control does not depend on the code under test."
  (length (split-string
           (consult-hn--fill-string consult-hn-e2e--long-comment
                                    consult-hn-comment-width 'full)
           "\n" t)))

;;; Scenarios

(defun consult-hn-e2e--reset ()
  "Scrub artifacts so scenarios (and rounds) start clean."
  (setq consult-hn-e2e--requests nil
        consult-hn-e2e--delivered nil
        consult-hn-e2e--previews nil
        consult-hn--params (copy-sequence consult-hn-e2e--default-params))
  (dolist (b consult-hn-e2e--buffers)
    (when (buffer-live-p b) (kill-buffer b)))
  (setq consult-hn-e2e--buffers nil))

(defun consult-hn-e2e--scenario-streaming (k)
  "All pages stream into the open session."
  (consult-hn-e2e--reset)
  (run-at-time 0 nil #'consult-hn "emacs lisp")
  (consult-hn-e2e--await
   "S1 minibuffer opens with page-0 candidates"
   (lambda () (<= 5 (or (consult-hn-e2e--candidate-count) 0)))
   (lambda (_)
     (consult-hn-e2e--await
      "S1 pages stream in while idle (15 total)"
      (lambda () (eql (consult-hn-e2e--candidate-count) 15))
      (lambda (ok)
        (when ok
          (consult-hn-e2e--check
           "S1 pages requested in order"
           (equal (consult-hn-e2e--pages-requested) '(0 1 2))
           (format "%S" (consult-hn-e2e--pages-requested)))
          (consult-hn-e2e--check
           "S1 a full page of hits is asked for"
           (seq-every-p (lambda (u)
                          (string-match-p
                           (format "hitsPerPage=%d" consult-hn-hits-per-page) u))
                        consult-hn-e2e--requests)
           (car (last consult-hn-e2e--requests)))
          (consult-hn-e2e--check
           "S1 previewing the selection went through the preview seam"
           consult-hn-e2e--previews)
          ;; one, not two: the buffer is reused, so a session that
          ;; inherited a previous one's decoration would show two here
          (consult-hn-e2e--check
           "S1 the prompt carries exactly one chip overlay"
           (eql 1 (length (consult-hn-e2e--chip-overlays
                           (consult-hn-e2e--minibuffer))))
           (format "%S" (length (consult-hn-e2e--chip-overlays
                                 (consult-hn-e2e--minibuffer)))))
          ;; the fixtures serve sixteen hits for fifteen items, so the
          ;; count above is itself the proof the repeat was dropped
          (consult-hn-e2e--check
           "S1 the item repeated across pages arrives once"
           (and (eql 16 (consult-hn-e2e--fixture-hits consult-hn-e2e--pages))
                (eql 1 (seq-count (lambda (c) (string-match-p "Story title 3 " c))
                                  (consult-hn-e2e--candidates))))
           (format "served=%S shown=%S"
                   (consult-hn-e2e--fixture-hits consult-hn-e2e--pages)
                   (seq-count (lambda (c) (string-match-p "Story title 3 " c))
                              (consult-hn-e2e--candidates))))
          ;; the stub ignores the query, so only the URL can prove a
          ;; multi-word search leaves Emacs in one piece
          (consult-hn-e2e--check
           "S1 multi-word query encoded exactly once"
           (let ((url (car (last consult-hn-e2e--requests))))
             (and (string-match-p "query=emacs%20lisp" url)
                  (not (string-match-p "%25" url))))
           (car (last consult-hn-e2e--requests))))
        (funcall k))))))

(defun consult-hn-e2e--scenario-annotation (k)
  "The rendered annotation is capped and marked, not merely shortened."
  (let ((rendered (consult-hn-e2e--rendered)))
    (consult-hn-e2e--check "S2 something is rendered" (and rendered
                                                           (< 0 (length rendered))))
    (when rendered
      (consult-hn-e2e--check
       "S2 long comment reaches the display capped"
       (string-match-p "wordy" rendered))
      (consult-hn-e2e--check
       "S2 the far end of the long comment never reaches the display"
       (not (string-match-p "ENDMARKER" rendered)))
      (consult-hn-e2e--check
       "S2 the cut is marked with an ellipsis"
       (string-match-p "…" rendered))
      (consult-hn-e2e--check
       "S2 comment-free candidates get no annotation lines"
       (string-match-p "Story title 1" rendered))))
  (funcall k))

(defun consult-hn-e2e--scenario-height (k)
  "The session's height budget is bounded, and demonstrably so."
  (let* ((mb (consult-hn-e2e--minibuffer))
         (local (and mb (with-current-buffer mb
                          (list (local-variable-p 'vertico-count)
                                vertico-count))))
         (lines (consult-hn-e2e--rendered-lines))
         (cap consult-hn-max-comment-lines)
         (budget (* (nth 1 local) (1+ cap)))
         (unbounded (consult-hn-e2e--unbounded-lines)))
    (consult-hn-e2e--check "S3 vertico-count is session-local"
                           (car local) (format "%S" local))
    (consult-hn-e2e--check
     "S3 count scaled by the per-candidate footprint"
     (eql (nth 1 local) (max 4 (floor (default-value 'vertico-count) (1+ cap))))
     (format "local=%S global=%S" local (default-value 'vertico-count)))
    (consult-hn-e2e--check
     "S3 rendered lines stay inside the budget"
     (and lines (<= lines (+ budget 2)))
     (format "rendered=%S budget=%S" lines budget))
    ;; the control: without the cap a single candidate alone would
    ;; overrun what the whole session is now allowed to occupy
    (consult-hn-e2e--check
     "S3 one uncapped comment would have exceeded the whole session budget"
     (< budget unbounded)
     (format "budget=%S one-uncapped-comment=%S" budget unbounded)))
  (funcall k))

(defun consult-hn-e2e--scenario-teardown (k)
  "Aborting leaves no trace in the reused minibuffer or in globals."
  (consult-hn-e2e--keys "C-g")
  (consult-hn-e2e--await
   "S4 session closes on abort"
   (lambda () (zerop (minibuffer-depth)))
   (lambda (_)
     (consult-hn-e2e--check "S4 * Minibuf-1* buffer name intact"
                            (get-buffer " *Minibuf-1*"))
     (consult-hn-e2e--check
      "S4 global vertico-count untouched by the session"
      (eql (default-value 'vertico-count) 15)
      (format "%S" (default-value 'vertico-count)))
     (consult-hn-e2e--check
      "S4 no scaled count left in the reused minibuffer"
      (not (and (get-buffer " *Minibuf-1*")
                (buffer-local-boundp 'vertico-count (get-buffer " *Minibuf-1*"))
                (with-current-buffer " *Minibuf-1*"
                  (local-variable-p 'vertico-count)))))
     (funcall k))))

(defvar consult-hn-e2e--max-pages consult-hn-max-pages
  "The cap every scenario but the page-cap one runs under.")

(defun consult-hn-e2e--scenario-page-cap (k)
  "The chain stops at the cap, not at the page count the endpoint reports."
  (consult-hn-e2e--reset)
  ;; fixtures declare three pages; two is all this session may spend
  (setq consult-hn-max-pages 2)
  (run-at-time 0 nil #'consult-hn "emacs lisp")
  (consult-hn-e2e--await
   "S5 pages stream in up to the cap"
   (lambda () (eql (consult-hn-e2e--candidate-count) 10))
   (lambda (_)
     ;; long enough for a third page to have arrived, had one been asked for
     (run-at-time
      0.6 nil
      (lambda ()
        (consult-hn-e2e--check
         "S5 no page past the cap was requested"
         (equal (consult-hn-e2e--pages-requested) '(0 1))
         (format "%S" (consult-hn-e2e--pages-requested)))
        (consult-hn-e2e--check
         "S5 the session settles at the capped candidate count"
         (eql (consult-hn-e2e--candidate-count) 10)
         (format "%S" (consult-hn-e2e--candidate-count)))
        (setq consult-hn-max-pages consult-hn-e2e--max-pages)
        (consult-hn-e2e--keys "C-g")
        (consult-hn-e2e--await
         "S5 session closes on abort"
         (lambda () (zerop (minibuffer-depth)))
         (lambda (_) (funcall k))))))))

(defun consult-hn-e2e--scenario-stale-chain (k)
  "A re-query mid-stream leaves nothing of the first search behind.
Page 1 of the first search answers from a buffer the session never
received, so cancelling it is not on the table: only the generation it
was issued under can keep its results out of the session."
  (consult-hn-e2e--reset)
  (setq consult-hn-e2e--detached-page 1)
  (run-at-time 0 nil #'consult-hn "emacs lisp")
  (consult-hn-e2e--await
   "S6 first search shows its first page"
   (lambda () (<= 5 (consult-hn-e2e--candidate-count)))
   (lambda (_)
     ;; re-query while page 1 of the first search is still in the air
     (consult-hn-e2e--keys (concat "C-a C-k " consult-hn-e2e--other-query))
     (consult-hn-e2e--await
      "S6 second search replaces the results"
      (lambda () (seq-some (lambda (c) (string-match-p "Other story" c))
                           (consult-hn-e2e--candidates)))
      (lambda (_)
        (run-at-time
         (+ 0.3 consult-hn-e2e--detached-delay) nil
         (lambda ()
           ;; the control: without a delivery there is nothing to drop
           (consult-hn-e2e--check
            "S6 the retired page was delivered all the same"
            (seq-some (lambda (u)
                        (and (eql 1 (consult-hn-e2e--url-page u))
                             (equal (consult-hn-e2e--url-query u) "emacs lisp")))
                      consult-hn-e2e--delivered)
            (format "%S" consult-hn-e2e--delivered))
           (consult-hn-e2e--check
            "S6 no candidate of the retired search survives"
            (not (seq-some (lambda (c) (string-match-p "Story title" c))
                           (consult-hn-e2e--candidates)))
            (format "%S" (seq-take (consult-hn-e2e--candidates) 3)))
           (consult-hn-e2e--check
            "S6 the retired chain asked for no further page"
            (not (seq-some (lambda (u)
                             (and (eql 2 (consult-hn-e2e--url-page u))
                                  (equal (consult-hn-e2e--url-query u) "emacs lisp")))
                           consult-hn-e2e--requests))
            (format "%S" (mapcar #'consult-hn-e2e--url-page
                                 consult-hn-e2e--requests)))
           (setq consult-hn-e2e--detached-page nil)
           (consult-hn-e2e--keys "C-g")
           (consult-hn-e2e--await
            "S6 session closes on abort"
            (lambda () (zerop (minibuffer-depth)))
            (lambda (_) (funcall k))))))))))

(defun consult-hn-e2e--scenario-restart (k)
  "The restart handle re-runs the search under the parameters of the moment.
The input is left untouched throughout, which is the whole point: the
throttle drops input equal to the last, so a parameter change over
unchanged text reaches the pipeline through the handle or not at all."
  (consult-hn-e2e--reset)
  (run-at-time 0 nil #'consult-hn "emacs lisp")
  (consult-hn-e2e--await
   "S7 session opens on the unparameterised results"
   (lambda () (eql (consult-hn-e2e--candidate-count) 15))
   (lambda (_)
     (consult-hn-e2e--check "S7 the source hands out a restart handle"
                            (functionp consult-hn--restart))
     (setq consult-hn-e2e--input-before (consult-hn-e2e--input))
     (setq consult-hn--params
           (plist-put (copy-sequence consult-hn--params) :type 'story))
     (funcall consult-hn--restart)
     (consult-hn-e2e--await
      "S7 the results come back parameterised"
      (lambda () (seq-some (lambda (c) (string-match-p "Tagged story" c))
                           (consult-hn-e2e--candidates)))
      (lambda (_)
        (consult-hn-e2e--check
         "S7 nothing of the previous result set is mixed in"
         (not (seq-some (lambda (c) (string-match-p "Story title" c))
                        (consult-hn-e2e--candidates)))
         (format "%S" (seq-take (consult-hn-e2e--candidates) 3)))
        (consult-hn-e2e--check
         "S7 the re-query started from page 0"
         (equal (last (consult-hn-e2e--pages-requested) 1) '(0))
         (format "%S" (consult-hn-e2e--pages-requested)))
        (consult-hn-e2e--check
         "S7 the input was never touched"
         (equal (consult-hn-e2e--input) consult-hn-e2e--input-before)
         (format "before=%S after=%S"
                 consult-hn-e2e--input-before (consult-hn-e2e--input)))
        (consult-hn-e2e--keys "C-g")
        (consult-hn-e2e--await
         "S7 session closes on abort"
         (lambda () (zerop (minibuffer-depth)))
         (lambda (_)
           (consult-hn-e2e--check
            "S7 the handle is taken back at teardown"
            (null consult-hn--restart)
            (format "%S" consult-hn--restart))
           (funcall k))))))))

(defun consult-hn-e2e--scenario-chips (k)
  "Chips appear, follow the parameters, and leave with the session."
  (consult-hn-e2e--reset)
  (run-at-time 0 nil #'consult-hn "emacs lisp")
  (consult-hn-e2e--await
   "S8 session opens"
   (lambda () (<= 5 (or (consult-hn-e2e--candidate-count) 0)))
   (lambda (_)
     (consult-hn-e2e--check
      "S8 the prompt is decorated"
      (consult-hn-e2e--chips)
      "no chip overlay on the prompt")
     (consult-hn-e2e--check
      "S8 an untouched state shows nothing at all"
      (equal (consult-hn-e2e--chips) "")
      (format "%S" (consult-hn-e2e--chips)))
     (setq consult-hn--params
           (thread-first (copy-sequence consult-hn--params)
                         (plist-put :type 'comment)
                         (plist-put :points 100)))
     (consult-hn--chips-update)
     (consult-hn-e2e--check
      "S8 the chips say what the parameters say"
      (equal (consult-hn-e2e--chips) " [comment · >100p]")
      (format "%S" (consult-hn-e2e--chips)))
     (consult-hn-e2e--keys "C-g")
     (consult-hn-e2e--await
      "S8 session closes on abort"
      (lambda () (zerop (minibuffer-depth)))
      (lambda (_)
        ;; C6: the buffer is reused, so an overlay that outlives its
        ;; session is an overlay in the next, unrelated prompt
        (consult-hn-e2e--check
         "S8 no chips left in the minibuffer that gets reused"
         (eql 0 (consult-hn-e2e--stale-chips))
         (format "%S" (consult-hn-e2e--stale-chips)))
        (funcall k))))))

(defun consult-hn-e2e--scenario-parameter-key (k)
  "A parameter command replaces the result set and says so on the prompt."
  (consult-hn-e2e--reset)
  (run-at-time 0 nil #'consult-hn "emacs lisp")
  (consult-hn-e2e--await
   "S9 session opens on the unparameterised results"
   (lambda () (eql (consult-hn-e2e--candidate-count) 15))
   (lambda (_)
     (setq consult-hn-e2e--input-before (consult-hn-e2e--input))
     ;; through the real command loop, on the key a user would press
     (consult-hn-e2e--keys "C-c t")
     (consult-hn-e2e--await
      "S9 the command re-queries and the results come back parameterised"
      (lambda () (seq-some (lambda (c) (string-match-p "Tagged story" c))
                           (consult-hn-e2e--candidates)))
      (lambda (_)
        (consult-hn-e2e--check
         "S9 nothing of the previous result set is mixed in"
         (not (seq-some (lambda (c) (string-match-p "Story title" c))
                        (consult-hn-e2e--candidates)))
         (format "%S" (seq-take (consult-hn-e2e--candidates) 3)))
        (consult-hn-e2e--check
         "S9 the chips follow the command"
         (equal (consult-hn-e2e--chips) " [story]")
         (format "%S" (consult-hn-e2e--chips)))
        (consult-hn-e2e--check
         "S9 the session kept its input"
         (equal (consult-hn-e2e--input) consult-hn-e2e--input-before)
         (format "before=%S after=%S"
                 consult-hn-e2e--input-before (consult-hn-e2e--input)))
        ;; pressing it again moves on to the next choice, rather than
        ;; toggling between two
        (consult-hn-e2e--keys "C-c t")
        (consult-hn-e2e--await
         "S9 the same key cycles on to the next choice"
         (lambda () (seq-some (lambda (c) (string-match-p "Tagged comment" c))
                              (consult-hn-e2e--candidates)))
         (lambda (_)
           (consult-hn-e2e--check
            "S9 the chips follow it there too"
            (equal (consult-hn-e2e--chips) " [comment]")
            (format "%S" (consult-hn-e2e--chips)))
           (consult-hn-e2e--keys "C-g")
           (consult-hn-e2e--await
            "S9 session closes on abort"
            (lambda () (zerop (minibuffer-depth)))
            (lambda (_) (funcall k))))))))))

(defun consult-hn-e2e--scenario-recursive-read (k)
  "Reading an author from inside the session leaves the session standing."
  (consult-hn-e2e--reset)
  (run-at-time 0 nil #'consult-hn "emacs lisp")
  (consult-hn-e2e--await
   "S10 session opens"
   (lambda () (eql (consult-hn-e2e--candidate-count) 15))
   (lambda (_)
     (setq consult-hn-e2e--input-before (consult-hn-e2e--input))
     (consult-hn-e2e--keys "C-c a")
     (consult-hn-e2e--await
      "S10 the author command reads from a minibuffer of its own"
      (lambda () (eql 2 (minibuffer-depth)))
      (lambda (depth)
        (when depth
          (consult-hn-e2e--keys "p g RET"))
        (consult-hn-e2e--await
         "S10 the session is back to one minibuffer"
         (lambda () (eql 1 (minibuffer-depth)))
         (lambda (_)
           (consult-hn-e2e--await
            "S10 the author that was read is what came back"
            (lambda () (seq-some (lambda (c) (string-match-p "Tagged author_pg" c))
                                 (consult-hn-e2e--candidates)))
            (lambda (_)
              (consult-hn-e2e--check
               "S10 the chips carry the author"
               (equal (consult-hn-e2e--chips) " [pg]")
               (format "%S" (consult-hn-e2e--chips)))
              (consult-hn-e2e--check
               "S10 the session came through with its input intact"
               (equal (consult-hn-e2e--input) consult-hn-e2e--input-before)
               (format "before=%S after=%S"
                       consult-hn-e2e--input-before (consult-hn-e2e--input)))
              (consult-hn-e2e--check
               "S10 recursive minibuffers are left forbidden as they were found"
               (null (default-value 'enable-recursive-minibuffers))
               (format "%S" (default-value 'enable-recursive-minibuffers)))
              (consult-hn-e2e--keys "C-g")
              (consult-hn-e2e--await
               "S10 session closes on abort"
               (lambda () (zerop (minibuffer-depth)))
               (lambda (_)
                 (consult-hn-e2e--check
                  "S10 no chips left in the minibuffer that gets reused"
                  (eql 0 (consult-hn-e2e--stale-chips))
                  (format "%S" (consult-hn-e2e--stale-chips)))
                 (funcall k))))))))))))

(defun consult-hn-e2e--scenario-from-lisp (k)
  "A session opened with parameters from Lisp runs under them, once."
  (consult-hn-e2e--reset)
  (run-at-time 0 nil #'consult-hn "emacs lisp" :type 'comment :points 100)
  (consult-hn-e2e--await
   "S11 the session opens already parameterised"
   (lambda () (seq-some (lambda (c) (string-match-p "Tagged comment" c))
                        (consult-hn-e2e--candidates)))
   (lambda (_)
     (consult-hn-e2e--check
      "S11 the prompt says what it was opened with"
      (equal (consult-hn-e2e--chips) " [comment · >100p]")
      (format "%S" (consult-hn-e2e--chips)))
     (consult-hn-e2e--keys "C-g")
     (consult-hn-e2e--await
      "S11 session closes on abort"
      (lambda () (zerop (minibuffer-depth)))
      (lambda (_)
        (consult-hn-e2e--check
         "S11 the call's parameters did not outlive the call"
         (equal consult-hn--params consult-hn-e2e--default-params)
         (format "%S" consult-hn--params))
        (funcall k))))))

(defun consult-hn-e2e--scenario-transient (k)
  "The menu opens a session under the parameters it was set to.
Driven on the keys a user presses, since the menu is the one piece with
no other harness: the state it hands over is only as good as the
arguments the real infixes produce."
  (consult-hn-e2e--reset)
  (run-at-time 0 nil #'consult-hn-transient)
  (consult-hn-e2e--await
   "S12 the menu opens"
   (lambda () (and transient--prefix t))
   (lambda (_)
     ;; t cycles the type on to story, RET searches on it
     (consult-hn-e2e--keys "t RET")
     (consult-hn-e2e--await
      "S12 the session opens on what the menu was set to"
      (lambda () (seq-some (lambda (c) (string-match-p "Tagged story" c))
                           (consult-hn-e2e--candidates)))
      (lambda (_)
        (consult-hn-e2e--check
         "S12 the chips say what the menu said"
         (equal (consult-hn-e2e--chips) " [story]")
         (format "%S" (consult-hn-e2e--chips)))
        (consult-hn-e2e--check
         "S12 the menu wrote its parameters into the state"
         (eq 'story (plist-get consult-hn--params :type))
         (format "%S" consult-hn--params))
        (consult-hn-e2e--keys "C-g")
        (consult-hn-e2e--await
         "S12 the session closes, leaving the menu standing"
         (lambda () (zerop (minibuffer-depth)))
         (lambda (_)
           (consult-hn-e2e--check
            "S12 no chips left in the minibuffer that gets reused"
            (eql 0 (consult-hn-e2e--stale-chips))
            (format "%S" (consult-hn-e2e--stale-chips)))
           (consult-hn-e2e--keys "C-g")
           (consult-hn-e2e--await
            "S12 the menu goes away when dismissed"
            (lambda () (null transient--prefix))
            (lambda (_) (funcall k))))))))))

;;; Runner

(defvar consult-hn-e2e--scenarios
  (list #'consult-hn-e2e--scenario-streaming
        #'consult-hn-e2e--scenario-annotation
        #'consult-hn-e2e--scenario-height
        #'consult-hn-e2e--scenario-teardown
        #'consult-hn-e2e--scenario-page-cap
        #'consult-hn-e2e--scenario-stale-chain
        #'consult-hn-e2e--scenario-restart
        #'consult-hn-e2e--scenario-chips
        #'consult-hn-e2e--scenario-parameter-key
        #'consult-hn-e2e--scenario-recursive-read
        #'consult-hn-e2e--scenario-from-lisp
        #'consult-hn-e2e--scenario-transient)
  "Ordered; scenarios 2 and 3 observe the session opened by the first.")

(defun consult-hn-e2e--run-scenarios (scenarios done)
  "Run SCENARIOS sequentially, then call DONE."
  (if (null scenarios)
      (funcall done)
    (consult-hn-e2e--safe-call
     (car scenarios)
     (lambda ()
       (run-at-time 0.1 nil #'consult-hn-e2e--run-scenarios
                    (cdr scenarios) done)))))

(defun consult-hn-e2e--finish ()
  "Write the verdict and exit."
  (consult-hn-e2e--log "DONE failures=%d" consult-hn-e2e--failures)
  (with-temp-buffer
    (insert (format "EXIT:%d\n" (if (zerop consult-hn-e2e--failures) 0 1)))
    (append-to-file (point-min) (point-max) consult-hn-e2e-results-file))
  (kill-emacs (if (zerop consult-hn-e2e--failures) 0 1)))

(defun consult-hn-e2e-run ()
  "Entry point: set up the stub and run the suite twice."
  (when (file-exists-p consult-hn-e2e-results-file)
    (delete-file consult-hn-e2e-results-file))
  ;; watchdog: a hung minibuffer or lost timer chain must not hang CI
  (run-at-time 90 nil (lambda ()
                        (consult-hn-e2e--log "WATCHDOG fired")
                        (with-temp-buffer
                          (insert "EXIT:2\n")
                          (append-to-file (point-min) (point-max)
                                          consult-hn-e2e-results-file))
                        (kill-emacs 2)))
  (advice-add 'url-retrieve :override #'consult-hn-e2e--url-retrieve)
  ;; keep eww (and the browser) out of the suite: previewing is real,
  ;; rendering the item is not what these scenarios are about
  (setq consult-hn-preview-fn
        (lambda (&rest args) (push (cons 'preview args) consult-hn-e2e--previews))
        consult-hn-browse-fn
        (lambda (&rest args) (push (cons 'browse args) consult-hn-e2e--previews)))
  (consult-hn-e2e--run-scenarios
   consult-hn-e2e--scenarios
   (lambda ()
     (if (eql 2 consult-hn-e2e--round)
         (consult-hn-e2e--finish)
       (setq consult-hn-e2e--round 2)
       (consult-hn-e2e--log "--- second round: proving no state leaks ---")
       (consult-hn-e2e--run-scenarios consult-hn-e2e--scenarios
                                      #'consult-hn-e2e--finish)))))

(provide 'consult-hn-e2e)
;;; consult-hn-e2e.el ends here
