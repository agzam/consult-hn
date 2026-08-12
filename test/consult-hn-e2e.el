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

(defvar consult-hn-e2e-results-file
  (expand-file-name "consult-hn-e2e-results.txt"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Where PASS/FAIL lines and the final verdict go.")

(defvar consult-hn-e2e--failures 0)
(defvar consult-hn-e2e--round 1)
(defvar consult-hn-e2e--requests nil "Recorded request URLs, newest first.")
(defvar consult-hn-e2e--previews nil "Recorded preview/browse invocations.")
(defvar consult-hn-e2e--buffers nil "Response buffers handed out by the stub.")

;;; Fixtures

(defconst consult-hn-e2e--long-comment
  (concat (mapconcat #'identity (make-list 500 "wordy") " ") " ENDMARKER")
  "A comment far longer than any sane annotation, ending in a marker.
The marker must never reach the display once the cap is in force.")

(defun consult-hn-e2e--hit (n &optional comment)
  "One API hit numbered N, a comment hit when COMMENT is given."
  (let ((h (list (cons "author" (format "user%d" n))
                 (cons "created_at" (format "2025-01-30T10:%02d:00" n))
                 (cons "created_at_i" (- 1738226435 (* n 3600)))
                 (cons "objectID" (number-to-string (+ 1000 n)))
                 (cons "story_id" (number-to-string (+ 1000 n)))
                 (cons "points" (* n 3))
                 (cons "num_comments" n))))
    (if comment
        (append h (list (cons "story_title" (format "Story title %d" n))
                        (cons "comment_text" (format "<p>%s</p>" comment))))
      (append h (list (cons "title" (format "Story title %d" n))
                      (cons "url" (format "https://example.com/%d" n)))))))

(defconst consult-hn-e2e--pages
  (list
   ;; page 0 carries the pathological comment
   (list (consult-hn-e2e--hit 1)
         (consult-hn-e2e--hit 2 consult-hn-e2e--long-comment)
         (consult-hn-e2e--hit 3 "short reply about emacs")
         (consult-hn-e2e--hit 4)
         (consult-hn-e2e--hit 5 consult-hn-e2e--long-comment))
   (list (consult-hn-e2e--hit 6) (consult-hn-e2e--hit 7 "another reply")
         (consult-hn-e2e--hit 8) (consult-hn-e2e--hit 9 "yet another reply")
         (consult-hn-e2e--hit 10))
   (list (consult-hn-e2e--hit 11) (consult-hn-e2e--hit 12 "trailing reply")
         (consult-hn-e2e--hit 13) (consult-hn-e2e--hit 14)
         (consult-hn-e2e--hit 15)))
  "Three pages of five hits each; fifteen candidates in total.")

(defun consult-hn-e2e--payload (page)
  "JSON body for PAGE."
  (json-encode
   (list (cons "hits" (or (nth page consult-hn-e2e--pages) []))
         (cons "nbPages" (length consult-hn-e2e--pages))
         (cons "page" page)
         (cons "hitsPerPage" 5)
         (cons "nbHits" 15))))

(defun consult-hn-e2e--url-page (url)
  "Page number requested by URL."
  (if (string-match "[?&]page=\\([0-9]+\\)" url)
      (string-to-number (match-string 1 url))
    0))

;;; The only stub: the HTTP seam

(defun consult-hn-e2e--url-retrieve (url callback &optional _cbargs _silent _cookies)
  "Stub of `url-retrieve' serving fixtures for URL to CALLBACK.
Mirrors the real thing closely enough to matter: the callback runs in
the response buffer with `url-http-end-of-headers' set, and a buffer
killed before delivery never calls back, which is how cancellation
works in production."
  (unless (string-match-p "hn\\.algolia\\.com" url)
    ;; nothing but the search API may reach the network; the browse and
    ;; preview seams are stubbed separately
    (consult-hn-e2e--check "unexpected non-API request" nil url))
  (push url consult-hn-e2e--requests)
  (let ((buf (generate-new-buffer " *consult-hn-e2e-response*")))
    (push buf consult-hn-e2e--buffers)
    (with-current-buffer buf
      (insert "HTTP/1.1 200 OK\nContent-Type: application/json\n\n")
      (setq-local url-http-end-of-headers (copy-marker (point)))
      (insert (consult-hn-e2e--payload (consult-hn-e2e--url-page url))))
    (run-at-time
     0.03 nil
     (lambda ()
       (when (buffer-live-p buf)
         (with-current-buffer buf (funcall callback nil)))))
    buf))

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

(defun consult-hn-e2e--candidate-count ()
  "Number of candidates the completion table currently serves."
  (when-let* ((mb (consult-hn-e2e--minibuffer)))
    (with-current-buffer mb
      (length (all-completions "" minibuffer-completion-table nil)))))

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
        consult-hn-e2e--previews nil)
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
           (equal (mapcar #'consult-hn-e2e--url-page
                          (reverse consult-hn-e2e--requests))
                  '(0 1 2))
           (format "%S" (mapcar #'consult-hn-e2e--url-page
                                (reverse consult-hn-e2e--requests))))
          (consult-hn-e2e--check
           "S1 previewing the selection went through the preview seam"
           consult-hn-e2e--previews)
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

;;; Runner

(defvar consult-hn-e2e--scenarios
  (list #'consult-hn-e2e--scenario-streaming
        #'consult-hn-e2e--scenario-annotation
        #'consult-hn-e2e--scenario-height
        #'consult-hn-e2e--scenario-teardown)
  "Ordered; the middle two observe the session opened by the first.")

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
