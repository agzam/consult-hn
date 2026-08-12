;;; consult-hn-tests.el --- tests for consult-hn.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: January 30, 2025
;; Keywords: search extensions
;; Homepage: https://github.com/agzam/consult-hn
;; Package-Requires: ((emacs "29"))
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'buttercup)
(require 'consult-hn)
(require 'consult-hn-transient)

(describe "consult-hn--fill-string"
  (it "wraps long strings"
    (expect (consult-hn--fill-string "This is a very long string that should definitely wrap")
            :to-match "\n"))

  (it "respects width parameter"
    (expect (length (car (split-string (consult-hn--fill-string "Long string here" 10) "\n")))
            :to-be-less-than 11))

  (it "handles empty strings"
    (expect (consult-hn--fill-string "")
            :to-equal ""))

  (it "handles justification"
    (expect (consult-hn--fill-string "test" 10 'center)
            :to-match "   test")))

(defconst consult-hn-tests--now 1738226435
  "Fixed clock for the specs that render a time range.")

(defun consult-hn-tests--params (&rest overrides)
  "The default parameter state with OVERRIDES applied."
  (let ((params (copy-sequence consult-hn--params)))
    (cl-loop for (k v) on overrides by #'cddr
             do (setq params (plist-put params k v)))
    params))

(defun consult-hn-tests--typed (type &rest overrides)
  "The default state carrying TYPE, plus any OVERRIDES.
Emacs 29 looks a keyword up in the lexical environment before treating
it as self-evaluating, and buttercup evaluates every expectation inside
an oclosure whose environment binds `:type' to the oclosure's own type
name.  A literal `:type' written inside `expect' is therefore read as
`buttercup--thunk' there, and the expectation quietly tests something
else.  Specs say it through here so they mean what they say."
  (apply #'consult-hn-tests--params :type type overrides))

(defun consult-hn-tests--type (&optional params)
  "The type PARAMS carry, or the live state's.
Reads the keyword outside any expectation; see `consult-hn-tests--typed'."
  (plist-get (or params consult-hn--params) :type))

(describe "consult-hn--params-render"
  (defvar consult-hn-tests--defaults)
  (before-each
    (spy-on 'time-convert :and-return-value consult-hn-tests--now)
    (setq consult-hn-tests--defaults consult-hn-default-search-params
          consult-hn-default-search-params nil))
  (after-each
    (setq consult-hn-default-search-params consult-hn-tests--defaults))

  (it "renders nothing but the page size for an untouched state"
    (expect (consult-hn--params-render (consult-hn-tests--params))
            :to-equal '((hitsPerPage 100))))

  (it "passes the query through raw, for the query builder to encode"
    (expect (consult-hn--params-render (consult-hn-tests--params :query "c++ & rust"))
            :to-equal '((query "c++ & rust") (hitsPerPage 100))))

  (it "drops a blank query"
    (expect (consult-hn--params-render (consult-hn-tests--params :query "  "))
            :to-equal '((hitsPerPage 100))))

  (it "maps :type to a tag, and `all' to no tag"
    (expect (alist-get 'tags (consult-hn--params-render
                              (consult-hn-tests--typed 'story)))
            :to-equal '("story"))
    (expect (alist-get 'tags (consult-hn--params-render
                              (consult-hn-tests--typed 'comment)))
            :to-equal '("comment"))
    (expect (alist-get 'tags (consult-hn--params-render
                              (consult-hn-tests--typed 'all)))
            :to-be nil))

  (it "maps :author to an author tag"
    (expect (alist-get 'tags (consult-hn--params-render
                              (consult-hn-tests--params :author "pg")))
            :to-equal '("author_pg")))

  (it "maps :front-page to the front_page tag"
    (expect (alist-get 'tags (consult-hn--params-render
                              (consult-hn-tests--params :front-page t)))
            :to-equal '("front_page")))

  (it "maps :points to a numeric filter"
    (expect (alist-get 'numericFilters
                       (consult-hn--params-render
                        (consult-hn-tests--params :points 100)))
            :to-equal '("points>100")))

  (it "maps :comments to a numeric filter"
    (expect (alist-get 'numericFilters
                       (consult-hn--params-render
                        (consult-hn-tests--params :comments 25)))
            :to-equal '("num_comments>25")))

  (it "maps every :range to a cutoff, and `all' to none"
    (dolist (row '((24h . 86400) (week . 604800)
                   (month . 2592000) (year . 31536000)))
      (expect (alist-get 'numericFilters
                         (consult-hn--params-render
                          (consult-hn-tests--params :range (car row))))
              :to-equal (list (format "created_at_i>%d"
                                      (- consult-hn-tests--now (cdr row))))))
    (expect (alist-get 'numericFilters
                       (consult-hn--params-render
                        (consult-hn-tests--params :range 'all)))
            :to-be nil))

  (it "maps :url-match to the searchable attribute restriction"
    (expect (alist-get 'restrictSearchableAttributes
                       (consult-hn--params-render
                        (consult-hn-tests--params :url-match t)))
            :to-equal '("url")))

  (it "leaves :sort to the endpoint, never to the parameters"
    (expect (consult-hn--params-render (consult-hn-tests--params :sort 'relevance))
            :to-equal '((hitsPerPage 100))))

  (it "appends the page number only when one is asked for"
    (expect (alist-get 'page (consult-hn--params-render
                              (consult-hn-tests--params :query "foo")))
            :to-be nil)
    (expect (alist-get 'page (consult-hn--params-render
                              (consult-hn-tests--params :query "foo") 0))
            :to-equal '(0))
    (expect (alist-get 'page (consult-hn--params-render
                              (consult-hn-tests--params :query "foo") 3))
            :to-equal '(3)))

  ;; combinations: the joining is where a naive mapping falls apart
  (it "joins tags with a comma"
    (expect (alist-get 'tags (consult-hn--params-render
                              (consult-hn-tests--typed
                               'comment :author "pg" :front-page t)))
            :to-equal '("comment,author_pg,front_page")))

  (it "joins every numeric condition into one parameter"
    (expect (alist-get 'numericFilters
                       (consult-hn--params-render
                        (consult-hn-tests--params
                         :points 100 :comments 25 :range '24h)))
            :to-equal (list (format "points>100,num_comments>25,created_at_i>%d"
                                    (- consult-hn-tests--now 86400)))))

  (it "renders a fully specified state"
    (expect (consult-hn--params-render
             (consult-hn-tests--typed
              'story :query "emacs lisp" :author "pg" :points 100
              :comments 25 :range 'week :front-page t :url-match t
              :sort 'relevance)
             2)
            :to-equal
            `((query "emacs lisp")
              (tags "story,author_pg,front_page")
              (numericFilters ,(format "points>100,num_comments>25,created_at_i>%d"
                                       (- consult-hn-tests--now 604800)))
              (restrictSearchableAttributes "url")
              (hitsPerPage 100)
              (page 2))))

  (it "lets user defaults fill what the state leaves unset"
    (setq consult-hn-default-search-params '((typoTolerance false) (hitsPerPage 25)))
    (expect (consult-hn--params-render (consult-hn-tests--params :query "foo"))
            :to-equal '((query "foo") (typoTolerance false) (hitsPerPage 25))))

  (it "lets the state win over user defaults"
    (setq consult-hn-default-search-params '((tags "comment")))
    (expect (alist-get 'tags (consult-hn--params-render
                              (consult-hn-tests--typed 'story)))
            :to-equal '("story")))

  (it "drops user defaults the API would not accept"
    (setq consult-hn-default-search-params '((zop "120")))
    (expect (consult-hn--params-render (consult-hn-tests--params))
            :to-equal '((hitsPerPage 100)))))

(describe "consult-hn--params-endpoint"
  (it "sorts by date by default"
    (expect (consult-hn--params-endpoint (consult-hn-tests--params :query "foo"))
            :to-equal "search_by_date"))

  (it "ranks the front page by relevance, as it always has"
    (expect (consult-hn--params-endpoint (consult-hn-tests--params :front-page t))
            :to-equal "search"))

  (it "honours an explicit :sort over the front-page inference"
    (expect (consult-hn--params-endpoint
             (consult-hn-tests--params :front-page t :sort 'date))
            :to-equal "search_by_date")
    (expect (consult-hn--params-endpoint (consult-hn-tests--params :sort 'relevance))
            :to-equal "search"))

  (it "infers from the parameters actually going out"
    ;; the legacy input syntax reaches the endpoint through tags alone
    (expect (consult-hn--params-endpoint (consult-hn-tests--params)
                                         '((tags "story,front_page")))
            :to-equal "search")))

(describe "consult-hn--params-chips"
  (it "renders nothing at all for an untouched state"
    ;; what keeps the prompt unchanged for someone who never touches a
    ;; parameter, and what tells the overlay there is nothing to show
    (expect (consult-hn--params-chips (consult-hn-tests--params)) :to-equal "")
    (expect (consult-hn--params-chips (consult-hn-tests--params :query "emacs"))
            :to-equal "")
    (expect (consult-hn--params-chips (consult-hn-tests--params :author "  "))
            :to-equal ""))

  (it "renders one chip per set parameter"
    (dolist (row '((:type story "story")
                   (:type comment "comment")
                   (:author "pg" "pg")
                   (:points 100 ">100p")
                   (:comments 25 ">25c")
                   (:range 24h "24h")
                   (:range week "7d")
                   (:range month "30d")
                   (:range year "1y")
                   (:front-page t "front")
                   (:url-match t "url")
                   (:sort relevance "rel")
                   (:sort date "date")))
      (expect (consult-hn--params-chips
               (apply #'consult-hn-tests--params (butlast row)))
              :to-equal (format " [%s]" (car (last row))))))

  (it "says nothing about what is left at its default"
    (expect (consult-hn--params-chips (consult-hn-tests--typed 'all))
            :to-equal "")
    (expect (consult-hn--params-chips (consult-hn-tests--params :range 'all))
            :to-equal "")
    (expect (consult-hn--params-chips (consult-hn-tests--params :sort nil))
            :to-equal ""))

  (it "renders a full house in the order of the parameter model"
    (expect (consult-hn--params-chips
             (consult-hn-tests--typed
              'story :query "emacs lisp" :author "pg" :points 100
              :comments 25 :range 'week :front-page t :url-match t
              :sort 'relevance))
            :to-equal " [story · pg · >100p · >25c · 7d · front · url · rel]"))

  (it "carries a face, so the chips read as prompt decoration"
    (expect (get-text-property
             1 'face (consult-hn--params-chips
                      (consult-hn-tests--typed 'story)))
            :to-be 'consult-narrow-indicator)))

(describe "consult-hn--api-url"
  (it "encodes a multi-word query exactly once"
    ;; a two-word query used to go out as query=a%2520b and match nothing
    (let* ((params (consult-hn-tests--params :query "elpaca emacs"))
           (url (consult-hn--api-url params (consult-hn--params-render params 0))))
      (expect url :to-match "query=elpaca%20emacs")
      (expect url :not :to-match "%25")))

  (it "encodes query punctuation exactly once"
    (let* ((params (consult-hn-tests--params :query "c++ & rust"))
           (url (consult-hn--api-url params (consult-hn--params-render params))))
      (expect url :not :to-match "%25")))

  (it "puts the state on the endpoint the state asks for"
    (let* ((params (consult-hn-tests--params :query "foo" :front-page t))
           (url (consult-hn--api-url params (consult-hn--params-render params 1))))
      (expect url :to-match "/api/v1/search\\?")
      (expect url :to-match "tags=front_page")
      (expect url :to-match "page=1"))))

(describe "consult-hn--legacy-pairs"
  ;; the ` -- key=value' suffix is undocumented and unwarned, but the
  ;; package is published and someone may have it in a keybinding
  (it "finds nothing in an input that spells out no parameters"
    (expect (consult-hn--legacy-pairs "foo") :to-be nil)
    (expect (consult-hn--legacy-pairs "foo --") :to-be nil))

  (it "reads the pairs the suffix spells out"
    (expect (consult-hn--legacy-pairs "foo -- tags=story,author_boo")
            :to-equal '((tags "story,author_boo")))
    (expect (consult-hn--legacy-pairs "foo -- tags=(story,author_boo)")
            :to-equal '((tags "(story,author_boo)")))
    (expect (consult-hn--legacy-pairs "foo -- tags=story numericFilters=points>10")
            :to-equal '((tags "story") (numericFilters "points>10"))))

  (it "reads them with no query in front of them at all"
    (expect (consult-hn--legacy-pairs "-- tags=front_page")
            :to-equal '((tags "front_page")))))

(describe "consult-hn--request-url"
  (it "sends the query from the input under the current state"
    (let ((consult-hn--params (consult-hn-tests--params :type 'story)))
      (expect (consult-hn--request-url "elpaca emacs" 0)
              :to-match "query=elpaca%20emacs")
      (expect (consult-hn--request-url "elpaca emacs" 0) :to-match "tags=story")
      (expect (consult-hn--request-url "elpaca emacs" 0) :to-match "page=0")))

  (it "encodes a multi-word query exactly once"
    (expect (consult-hn--request-url "elpaca emacs" 0) :not :to-match "%25"))

  (it "layers the legacy suffix over the state"
    (let ((consult-hn--params (consult-hn-tests--params :type 'story)))
      ;; what the caller spelled out by hand replaces what the state says
      (expect (consult-hn--request-url "foo -- tags=comment" 0)
              :to-match "tags=comment")
      (expect (consult-hn--request-url "foo -- tags=comment" 0)
              :not :to-match "tags=story")))

  (it "keeps the query out of the legacy suffix"
    (expect (consult-hn--request-url "foo -- tags=story" 0) :to-match "query=foo"))

  (it "drops a legacy key the API would not accept"
    (expect (consult-hn--request-url "foo -- zop=120" 0) :not :to-match "zop"))

  (it "keeps the front_page endpoint inference for a legacy tag"
    ;; the state knows nothing of front-page here; only the tag says so
    (expect (consult-hn--request-url "foo -- tags=front_page" 0)
            :to-match "/api/v1/search\\?")
    (expect (consult-hn--request-url "foo -- tags=story" 0)
            :to-match "/api/v1/search_by_date\\?"))

  (it "leaves the parameter state alone"
    (let ((consult-hn--params (consult-hn-tests--params :type 'story)))
      (consult-hn--request-url "foo" 0)
      (expect (plist-get consult-hn--params :query) :to-be nil))))

(defun consult-hn-tests--payload (page nb-pages ids)
  "JSON body for PAGE of NB-PAGES carrying a hit per objectID in IDS."
  (json-encode
   (list (cons "hits"
               (mapcar (lambda (id)
                         (list (cons "objectID" id)
                               (cons "story_id" id)
                               (cons "title" (format "Title %s" id))
                               (cons "author" "someone")
                               (cons "created_at" "2025-01-30T10:00:00")
                               (cons "created_at_i" 1738226435)))
                       ids))
         (cons "nbPages" nb-pages)
         (cons "page" page))))

(defun consult-hn-tests--deliver (callback json)
  "Run CALLBACK in a buffer shaped like a finished response carrying JSON."
  (let ((buf (generate-new-buffer " *consult-hn-test-response*")))
    (unwind-protect
        (with-current-buffer buf
          (insert "HTTP/1.1 200 OK\nContent-Type: application/json\n\n")
          (setq-local url-http-end-of-headers (copy-marker (point)))
          (insert json)
          (funcall callback nil))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(describe "consult-hn--fetch-page-async"
  ;; the seam is `url-retrieve': the spy records the URL and hands back
  ;; the callback, which the spec then feeds a response by hand
  (defvar consult-hn-tests--urls)
  (defvar consult-hn-tests--callbacks)
  (before-each
    (setq consult-hn-tests--urls nil
          consult-hn-tests--callbacks nil
          consult-hn--seen (make-hash-table :test 'equal))
    (spy-on 'url-retrieve :and-call-fake
            (lambda (url callback &rest _)
              (push url consult-hn-tests--urls)
              (push callback consult-hn-tests--callbacks)
              nil)))

  (it "chains to the next page while the endpoint has more"
    (let ((consult-hn-max-pages 10)
          (delivered nil))
      (consult-hn--fetch-page-async "emacs" 0 (lambda (rows) (push rows delivered))
                                    consult-hn--generation)
      (consult-hn-tests--deliver (car consult-hn-tests--callbacks)
                                 (consult-hn-tests--payload 0 3 '("1" "2")))
      (expect (length delivered) :to-equal 1)
      (expect (length consult-hn-tests--urls) :to-equal 2)
      (expect (car consult-hn-tests--urls) :to-match "page=1")))

  (it "stops the chain at the page cap, whatever the endpoint reports"
    (let ((consult-hn-max-pages 2))
      (consult-hn--fetch-page-async "emacs" 0 #'ignore consult-hn--generation)
      ;; the endpoint offers 50 pages; two is all this chain may cost
      (consult-hn-tests--deliver (car consult-hn-tests--callbacks)
                                 (consult-hn-tests--payload 0 50 '("1")))
      (expect (length consult-hn-tests--urls) :to-equal 2)
      (consult-hn-tests--deliver (car consult-hn-tests--callbacks)
                                 (consult-hn-tests--payload 1 50 '("2")))
      (expect (length consult-hn-tests--urls) :to-equal 2)))

  (it "stops when the endpoint runs out of pages before the cap"
    (let ((consult-hn-max-pages 10))
      (consult-hn--fetch-page-async "emacs" 0 #'ignore consult-hn--generation)
      (consult-hn-tests--deliver (car consult-hn-tests--callbacks)
                                 (consult-hn-tests--payload 0 1 '("1")))
      (expect (length consult-hn-tests--urls) :to-equal 1)))

  (it "asks for a full page of hits"
    (consult-hn--fetch-page-async "emacs" 0 #'ignore consult-hn--generation)
    (expect (car consult-hn-tests--urls)
            :to-match (format "hitsPerPage=%d" consult-hn-hits-per-page)))

  (it "drops a page whose search has been superseded"
    (let ((consult-hn--generation 7)
          (delivered nil))
      (consult-hn--fetch-page-async "emacs" 0 (lambda (rows) (push rows delivered))
                                    consult-hn--generation)
      ;; the session moved on while this page was in the air
      (setq consult-hn--generation 8)
      (consult-hn-tests--deliver (car consult-hn-tests--callbacks)
                                 (consult-hn-tests--payload 0 3 '("1" "2")))
      (expect delivered :to-be nil)
      ;; and it must not drag the rest of its chain along either
      (expect (length consult-hn-tests--urls) :to-equal 1)))

  (it "delivers a page whose search is still current"
    (let ((consult-hn--generation 7)
          (delivered nil))
      (consult-hn--fetch-page-async "emacs" 0 (lambda (rows) (push rows delivered))
                                    consult-hn--generation)
      (consult-hn-tests--deliver (car consult-hn-tests--callbacks)
                                 (consult-hn-tests--payload 0 3 '("1" "2")))
      (expect (length (car delivered)) :to-equal 2))))

(describe "consult-hn--dedup"
  (before-each
    (setq consult-hn--seen (make-hash-table :test 'equal)))

  (it "drops an item this search has already delivered"
    (let ((rows (list (propertize "a" 'object-id "1")
                      (propertize "b" 'object-id "2"))))
      (expect (length (consult-hn--dedup rows)) :to-equal 2)
      ;; the same item again, on a later page of the same chain
      (expect (consult-hn--dedup (list (propertize "a" 'object-id "1")))
              :to-equal nil)))

  (it "drops a repeat inside a single page"
    (expect (length (consult-hn--dedup (list (propertize "a" 'object-id "1")
                                             (propertize "a" 'object-id "1"))))
            :to-equal 1))

  (it "keeps an item nothing identifies"
    (expect (length (consult-hn--dedup (list "no properties here")))
            :to-equal 1))

  (it "forgets everything once a new search starts"
    (spy-on 'url-retrieve :and-return-value nil)
    (consult-hn--dedup (list (propertize "a" 'object-id "1")))
    (funcall (consult-hn--async-source #'ignore) "emacs")
    (expect (length (consult-hn--dedup (list (propertize "a" 'object-id "1"))))
            :to-equal 1)))

(describe "consult-hn--async-source"
  (it "retires the pages in flight on every restart"
    (spy-on 'url-retrieve :and-return-value nil)
    (let* ((consult-hn--generation 0)
           (source (consult-hn--async-source #'ignore))
           (first (progn (funcall source "emacs") consult-hn--generation)))
      (funcall source "emacs lisp")
      (expect consult-hn--generation :not :to-equal first)))

  (it "retires the pages in flight when the session ends"
    (spy-on 'url-retrieve :and-return-value nil)
    (let* ((consult-hn--generation 0)
           (source (consult-hn--async-source #'ignore)))
      (funcall source "emacs")
      (let ((live consult-hn--generation))
        (funcall source 'destroy)
        (expect consult-hn--generation :not :to-equal live))))

  (it "kills the request buffers it knows about"
    (let* ((consult-hn--generation 0)
           (buffer (generate-new-buffer " *consult-hn-test-request*"))
           (source (consult-hn--async-source #'ignore)))
      (spy-on 'url-retrieve :and-return-value buffer)
      (funcall source "emacs")
      (expect (buffer-live-p buffer) :to-be t)
      (funcall source 'destroy)
      (expect (buffer-live-p buffer) :to-be nil)))

  (it "passes the lifecycle on to the stages downstream"
    ;; consult's own indicator deletes its prompt overlay on destroy and
    ;; the refresh stage cancels its timer; swallowing the action leaves
    ;; both behind in a minibuffer that gets reused
    (spy-on 'url-retrieve :and-return-value nil)
    (let* ((seen nil)
           (source (consult-hn--async-source (lambda (a) (push a seen)))))
      (funcall source 'setup)
      (funcall source 'destroy)
      (expect (reverse seen) :to-equal '(setup destroy)))))

(describe "consult-hn--restart"
  (defvar consult-hn-tests--urls)
  (before-each
    (setq consult-hn--restart nil
          consult-hn--seen (make-hash-table :test 'equal)
          consult-hn-tests--urls nil)
    (spy-on 'url-retrieve :and-call-fake
            (lambda (url &rest _) (push url consult-hn-tests--urls) nil)))
  (after-each
    (setq consult-hn--restart nil))

  (it "is handed out while the session lives and taken back at teardown"
    (let ((source (consult-hn--async-source #'ignore)))
      (expect consult-hn--restart :to-be nil)
      (funcall source 'setup)
      (expect (functionp consult-hn--restart) :to-be t)
      (funcall source 'destroy)
      (expect consult-hn--restart :to-be nil)))

  (it "runs the current input again under the parameters of the moment"
    (let* ((consult-hn--generation 0)
           (consult-hn--params (consult-hn-tests--params))
           (downstream nil)
           (source (consult-hn--async-source (lambda (a) (push a downstream)))))
      (funcall source 'setup)
      (funcall source "emacs")
      (setq consult-hn-tests--urls nil downstream nil)
      (puthash "1" t consult-hn--seen)
      ;; the input does not change, which is precisely what the throttle
      ;; refuses to carry; only the handle gets this into the pipeline
      (setq consult-hn--params (consult-hn-tests--params :type 'story))
      (let ((generation consult-hn--generation))
        (funcall consult-hn--restart)
        (expect consult-hn--generation :not :to-equal generation))
      (expect (hash-table-count consult-hn--seen) :to-equal 0)
      (expect downstream :to-equal '(flush))
      (expect (length consult-hn-tests--urls) :to-equal 1)
      (expect (car consult-hn-tests--urls) :to-match "query=emacs")
      (expect (car consult-hn-tests--urls) :to-match "tags=story")
      (expect (car consult-hn-tests--urls) :to-match "page=0")))

  (it "retires the pages of the search it replaces"
    (let* ((consult-hn--generation 0)
           (buffer (generate-new-buffer " *consult-hn-test-request*"))
           (source (consult-hn--async-source #'ignore)))
      (spy-on 'url-retrieve :and-return-value buffer)
      (funcall source 'setup)
      (funcall source "emacs")
      (funcall consult-hn--restart)
      (expect (buffer-live-p buffer) :to-be nil)))

  (it "asks for nothing when the session has no input worth searching"
    (let ((source (consult-hn--async-source #'ignore)))
      (funcall source 'setup)
      (funcall consult-hn--restart)
      (expect consult-hn-tests--urls :to-be nil)))

  (it "searches on the parameters alone before any input has arrived"
    ;; a parameter command can land between the session opening and the
    ;; first input reaching the source, and an author is a search anyway
    (let ((consult-hn--params (consult-hn-tests--params :author "pg"))
          (source (consult-hn--async-source #'ignore)))
      (funcall source 'setup)
      (funcall consult-hn--restart)
      (expect (length consult-hn-tests--urls) :to-equal 1)
      (expect (car consult-hn-tests--urls) :to-match "author_pg"))))

(describe "consult-hn--cycle"
  (it "walks the choices and wraps around at the end"
    (expect (consult-hn--cycle 'all '(all story comment)) :to-equal 'story)
    (expect (consult-hn--cycle 'comment '(all story comment)) :to-equal 'all)
    (expect (consult-hn--cycle nil '(nil date relevance)) :to-equal 'date)
    (expect (consult-hn--cycle 'relevance '(nil date relevance)) :to-be nil))

  (it "starts from the beginning for a value that is not a choice"
    (expect (consult-hn--cycle 'nonsense '(all story comment)) :to-equal 'all)))

(describe "consult-hn session commands"
  (defvar consult-hn-tests--restarts)
  (defvar consult-hn-tests--saved-params)
  (before-each
    (setq consult-hn-tests--restarts 0
          consult-hn-tests--saved-params consult-hn--params
          consult-hn--params (consult-hn-tests--params)
          consult-hn--restart (lambda () (setq consult-hn-tests--restarts
                                               (1+ consult-hn-tests--restarts))))
    (spy-on 'minibufferp :and-return-value t))
  (after-each
    (setq consult-hn--params consult-hn-tests--saved-params
          consult-hn--restart nil))

  (it "refuse to run outside a session"
    (spy-on 'minibufferp :and-return-value nil)
    (expect (consult-hn-session-type) :to-throw 'user-error))

  (it "cycle a parameter and search again on the spot"
    (consult-hn-session-type)
    (expect (consult-hn-tests--type) :to-equal 'story)
    (expect consult-hn-tests--restarts :to-equal 1)
    (consult-hn-session-type)
    (expect (consult-hn-tests--type) :to-equal 'comment)
    (expect consult-hn-tests--restarts :to-equal 2))

  (it "toggle a flag"
    (consult-hn-session-front-page)
    (expect (plist-get consult-hn--params :front-page) :to-be t)
    (consult-hn-session-front-page)
    (expect (plist-get consult-hn--params :front-page) :to-be nil)
    (consult-hn-session-url-match)
    (expect (plist-get consult-hn--params :url-match) :to-be t))

  (it "cycle the range and the sort through their own choices"
    (consult-hn-session-range)
    (expect (plist-get consult-hn--params :range) :to-equal '24h)
    (consult-hn-session-sort)
    (expect (plist-get consult-hn--params :sort) :to-equal 'date))

  (it "read an author, and release it again when told nothing"
    (spy-on 'consult-hn--read :and-return-value "pg")
    (consult-hn-session-author)
    (expect (plist-get consult-hn--params :author) :to-equal "pg")
    (spy-on 'consult-hn--read :and-return-value "")
    (consult-hn-session-author)
    (expect (plist-get consult-hn--params :author) :to-be nil))

  (it "read a threshold, and clear it when told nothing"
    (spy-on 'consult-hn--read :and-return-value "100")
    (consult-hn-session-points)
    (expect (plist-get consult-hn--params :points) :to-equal 100)
    (spy-on 'consult-hn--read :and-return-value "")
    (consult-hn-session-points)
    (expect (plist-get consult-hn--params :points) :to-be nil)
    (spy-on 'consult-hn--read :and-return-value "25")
    (consult-hn-session-comments)
    (expect (plist-get consult-hn--params :comments) :to-equal 25))

  (it "refuse a threshold that is not a number, leaving the search alone"
    (spy-on 'consult-hn--read :and-return-value "a lot")
    (expect (consult-hn-session-points) :to-throw 'user-error)
    (expect (plist-get consult-hn--params :points) :to-be nil)
    (expect consult-hn-tests--restarts :to-equal 0))

  (it "reach the request through the parameter state"
    (spy-on 'url-retrieve :and-return-value nil)
    (consult-hn-session-type)
    (expect (consult-hn--request-url "emacs" 0) :to-match "tags=story"))

  (it "are all reachable from the session keymap"
    (dolist (command '(consult-hn-session-type consult-hn-session-author
                       consult-hn-session-points consult-hn-session-comments
                       consult-hn-session-range consult-hn-session-front-page
                       consult-hn-session-url-match consult-hn-session-sort))
      (expect (where-is-internal command consult-hn-session-map)
              :not :to-be nil))))

(describe "consult-hn, called from Lisp"
  (defvar consult-hn-tests--read-args)
  (before-each
    (setq consult-hn-tests--read-args nil)
    (spy-on 'consult--read :and-call-fake
            (lambda (&rest args)
              ;; the session as it would be, observed from inside it:
              ;; the options it opens with, the state it runs under, and
              ;; the request it would issue for the input it was seeded
              ;; with (or for typed input, when it was seeded with none)
              (let* ((options (cdr args))
                     (initial (plist-get options :initial))
                     (input (if (string-blank-p initial) "emacs" initial)))
                (setq consult-hn-tests--read-args
                      (list :options options
                            :url (consult-hn--request-url input 0)
                            :params (copy-sequence consult-hn--params))))
              nil)))

  (it "sends a keyword parameter to the endpoint"
    (consult-hn "emacs" :type 'story :points 100 :author "pg")
    ;; decoded, because Emacs 29 hexifies the comma joining tags and
    ;; later versions leave it alone; either reaches the endpoint the same
    (let ((url (url-unhex-string (plist-get consult-hn-tests--read-args :url))))
      (expect url :to-match "tags=story,author_pg")
      (expect url :to-match "points")))

  (it "keeps the call's parameters out of the state that persists"
    (let ((before (copy-sequence consult-hn--params)))
      (consult-hn "emacs" :type 'story)
      (expect (plist-get consult-hn-tests--read-args :params)
              :not :to-equal before)
      (expect consult-hn--params :to-equal before)))

  (it "refuses a parameter the model does not have"
    (expect (consult-hn "emacs" :zop 1) :to-throw 'user-error))

  (it "starts the session on the query it was given"
    (consult-hn "emacs lisp")
    (expect (plist-get (plist-get consult-hn-tests--read-args :options) :initial)
            :to-equal "emacs lisp")
    ;; the legacy suffix is still a caller's to spell out
    (consult-hn "emacs -- tags=front_page")
    (expect (plist-get consult-hn-tests--read-args :url)
            :to-match "/api/v1/search\\?"))

  (it "passes none of it when called interactively"
    (let ((before (copy-sequence consult-hn--params)))
      (call-interactively #'consult-hn)
      (expect (plist-get (plist-get consult-hn-tests--read-args :options) :initial)
              :to-equal consult-hn-initial-input-string)
      (expect (plist-get consult-hn-tests--read-args :params) :to-equal before)))

  (it "still seeds the session from the obsolete seed variable"
    ;; obsolete, not removed: it is a published defcustom and somebody
    ;; has it set
    (let ((consult-hn-initial-input-string "lisp"))
      (call-interactively #'consult-hn)
      (expect (plist-get (plist-get consult-hn-tests--read-args :options) :initial)
              :to-equal "lisp")))

  (it "hands the session its parameter keymap"
    (consult-hn "emacs")
    (expect (plist-get (plist-get consult-hn-tests--read-args :options) :keymap)
            :to-be consult-hn-session-map)))

(describe "consult-hn--params-searchable-p"
  (it "says a query is needed when nothing else narrows the search"
    (expect (consult-hn--params-searchable-p (consult-hn-tests--params))
            :to-be nil)
    ;; where the query matches is not a search on its own
    (expect (consult-hn--params-searchable-p
             (consult-hn-tests--params :url-match t))
            :to-be nil)
    (expect (consult-hn--params-searchable-p
             (consult-hn-tests--params :sort 'relevance))
            :to-be nil))

  (it "says a parameter can carry the search by itself"
    (dolist (params '((:author "pg") (:type story) (:front-page t)
                      (:points 100) (:comments 25) (:range 24h)))
      (expect (consult-hn--params-searchable-p
               (apply #'consult-hn-tests--params params))
              :to-be t))))

(describe "the transient and the parameter state"
  (defvar consult-hn-tests--saved-params)
  (before-each
    (setq consult-hn-tests--saved-params consult-hn--params))
  (after-each
    (setq consult-hn--params consult-hn-tests--saved-params))

  (it "spells every parameter of the model as an argument"
    (expect (consult-hn-transient--args
             (consult-hn-tests--typed
              'story :query "emacs lisp" :author "pg" :points 100
              :comments 25 :range 'week :front-page t :url-match t
              :sort 'relevance))
            :to-equal '("--query=emacs lisp" "--type=story" "--author=pg"
                        "--points=100" "--num_comments=25" "--time=week"
                        "--front-page" "--url-match" "--sort=relevance")))

  (it "says nothing about a state left at its defaults"
    (expect (consult-hn-transient--args (consult-hn-tests--params)) :to-be nil)
    (expect (consult-hn-transient--args (consult-hn-tests--params :author "  "))
            :to-be nil))

  (it "reads every argument back into the state"
    (expect (consult-hn-transient--params
             '("--query=emacs" "--type=comment" "--author=pg" "--points=10"
               "--num_comments=5" "--time=24h" "--front-page" "--url-match"
               "--sort=date"))
            :to-equal '(:query "emacs" :type comment :author "pg" :points 10
                        :comments 5 :range 24h :front-page t :url-match t
                        :sort date)))

  (it "reads an empty menu back as the default state"
    (expect (consult-hn-transient--params nil)
            :to-equal (consult-hn-tests--params)))

  (it "round trips a full house, and the defaults"
    (dolist (params (list (consult-hn-tests--params)
                          (consult-hn-tests--params :type 'story)
                          (consult-hn-tests--params :range 'year :sort 'date)
                          (consult-hn-tests--params
                           :query "emacs lisp" :type 'comment :author "pg"
                           :points 100 :comments 25 :range 'month
                           :front-page t :url-match t :sort 'relevance)))
      (expect (consult-hn-transient--params
               (consult-hn-transient--args params))
              :to-equal params)))

  (it "takes a query that is a URL to mean matching on URLs"
    ;; what the menu has always done, now a rule rather than a variable
    ;; the query reader sets behind everyone's back
    (expect (plist-get (consult-hn-transient--params
                        '("--query=https://example.com/x"))
                       :url-match)
            :to-be t)
    (expect (plist-get (consult-hn-transient--params '("--query=example.com"))
                       :url-match)
            :to-be nil))

  (it "seeds its infixes from the state"
    (setq consult-hn--params (consult-hn-tests--params
                              :type 'comment :author "pg" :points 100
                              :front-page t))
    (dolist (row '((consult-hn-transient--type "--type=comment")
                   (consult-hn-transient--author "pg")
                   (consult-hn-transient--points "100")
                   (consult-hn-transient--front-page "--front-page")
                   (consult-hn-transient--range nil)))
      (let* ((obj (get (car row) 'transient--suffix))
             (saved (and (slot-boundp obj 'value) (oref obj value))))
        (unwind-protect
            (progn
              (funcall (oref obj init-value) obj)
              (expect (oref obj value) :to-equal (cadr row)))
          (oset obj value saved)))))

  (it "answers to both spellings of the key that searches"
    ;; graphical Emacs sends <return>, which arrives as RET only by
    ;; translation, and only while nothing else claims it: a `keymap'
    ;; text property claims it and beats even the menu's own map, which
    ;; is how a chat or prompt buffer leaves the one action unreachable
    (dolist (key '("RET" "<return>"))
      (let ((spec (transient-get-suffix 'consult-hn-transient key)))
        ;; the versions of transient Emacs bundles spell a suffix
        ;; specification differently, hence looking in both places
        (expect (or (plist-get (cdr spec) :command)
                    (plist-get (car (last spec)) :command))
                :to-equal 'consult-hn-transient-action))))

  (it "searches on what the menu says, and keeps it as the state"
    (spy-on 'transient-args :and-return-value '("--query=emacs" "--type=story"))
    (spy-on 'consult-hn)
    (consult-hn-transient-action)
    (expect (consult-hn-tests--type) :to-equal 'story)
    (expect (plist-get consult-hn--params :query) :to-equal "emacs")
    ;; the session takes the query from the state it was just handed
    (expect 'consult-hn :to-have-been-called-with)))

(describe "consult-hn--time-ago"
  ;; the float-time spy works this way: you add some time to a given
  ;; timestamp, faking (ts-now) to be sometime in the future
  (it "Matches HN display for known timestamps"
    (spy-on 'float-time :and-return-value (+ 1738226435 (* 4 24 60 60)))
    (expect (consult-hn--time-ago 1738226435) :to-equal "4 days ago")

    (spy-on 'float-time :and-return-value (+ 1738226435 (* 1 24 60 60)))
    (expect (consult-hn--time-ago 1738226435) :to-equal "1 days ago")

    (spy-on 'float-time :and-return-value (+ 1738226435 (* 16 60 60)))
    (expect (consult-hn--time-ago 1738226435) :to-equal "16 hours ago")

    (spy-on 'float-time :and-return-value (+ 1738226435 (* 16 60 60)))
    (expect (consult-hn--time-ago 1738226435) :to-equal "16 hours ago")

    (spy-on 'float-time :and-return-value (+ 1738226435 (* 60 60)))
    (expect (consult-hn--time-ago 1738226435) :to-equal "1 hours ago")

    (spy-on 'float-time :and-return-value (+ 1738226435 (* 125)))
    (expect (consult-hn--time-ago 1738226435) :to-equal "2 minutes ago"))

  (it "returns 'just now' for timestamps less than 60 seconds ago"
    (spy-on 'float-time :and-return-value 1738528340)
    (expect (consult-hn--time-ago 1738528300) :to-equal "just now")
    (expect (consult-hn--time-ago 1738528339) :to-equal "just now"))

  (it "returns minutes for timestamps less than an hour ago"
    (spy-on 'float-time :and-return-value 1738528340)
    (expect (consult-hn--time-ago 1738528040) :to-equal "5 minutes ago")
    (expect (consult-hn--time-ago 1738524740) :to-equal "1 hours ago"))

  (it "returns hours for timestamps less than a day ago"
    (spy-on 'float-time :and-return-value 1738528340)
    (expect (consult-hn--time-ago 1738485540) :to-equal "11 hours ago"))

  (it "returns days for timestamps more than a day ago"
    (spy-on 'float-time :and-return-value 1738528340)
    (expect (consult-hn--time-ago 1738355540) :to-equal "2 days ago")))

(describe "consult-hn--plist-keywordize"
  (it "converts simple plist keys to keywords"
    (expect (consult-hn--plist-keywordize '(foo "bar" zap "zop"))
            :to-equal '(:foo "bar" :zap "zop")))

  (it "handles empty plist"
    (expect (consult-hn--plist-keywordize '())
            :to-equal '()))

  (it "handles numeric values"
    (expect (consult-hn--plist-keywordize '(id 123 score 45))
            :to-equal '(:id 123 :score 45)))

  (it "preserves already keywordized elements"
    (expect (consult-hn--plist-keywordize '(:foo "bar" zap "zop"))
            :to-equal '(:foo "bar" :zap "zop"))))

(describe "consult-hn--parse-row-for-lookup"
  (it "parses a standard row with comment"
    (let ((row "Some Title Here   john_doe   2 hours ago   2025-01-30T10:00:00   This is a comment"))
      (expect (consult-hn--parse-row-for-lookup row)
              :to-equal '(:title "Some Title Here"
                          :author "john_doe"
                          :created-at "2025-01-30T10:00:00"
                          :comment "This is a comment"))))

  (it "parses a row without comment"
    (let ((row "Another Title   jane_smith   5 minutes ago   2025-01-30T11:00:00"))
      (expect (consult-hn--parse-row-for-lookup row)
              :to-equal '(:title "Another Title"
                          :author "jane_smith"
                          :created-at "2025-01-30T11:00:00"
                          :comment nil))))

  (it "handles titles with multiple spaces"
    (let ((row "Title  With  Spaces   user123   1 days ago   2025-01-29T10:00:00"))
      (expect (plist-get (consult-hn--parse-row-for-lookup row) :title)
              :to-equal "Title  With  Spaces")))

  (it "returns nil for malformed rows"
    (expect (consult-hn--parse-row-for-lookup "malformed row")
            :to-be nil)))

(describe "consult-hn--async-transform"
  (it "transforms collection with proper formatting"
    (let ((coll (list (propertize "Test Title" 
                                  'author "test_user"
                                  'ts 1738226435
                                  'created-at "2025-01-30T10:00:00"
                                  'comment nil))))
      (spy-on 'float-time :and-return-value (+ 1738226435 (* 2 60 60)))
      (let ((result (consult-hn--async-transform coll)))
        (expect (length result) :to-equal 1)
        (expect (car result) :to-match "Test Title")
        (expect (car result) :to-match "test_user")
        (expect (car result) :to-match "2 hours ago"))))

  (it "truncates long titles"
    (let* ((long-title (make-string 100 ?a))
           (coll (list (propertize long-title
                                   'author "user"
                                   'ts 1738226435
                                   'created-at "2025-01-30T10:00:00"
                                   'comment nil))))
      (spy-on 'float-time :and-return-value 1738226435)
      (let ((result (car (consult-hn--async-transform coll))))
        (expect (string-match-p "\\.\\.\\." result) :not :to-be nil))))

  (it "handles comments with invisible property"
    (let ((coll (list (propertize "Title"
                                  'author "user"
                                  'ts 1738226435
                                  'created-at "2025-01-30T10:00:00"
                                  'comment "This is a comment"))))
      (spy-on 'float-time :and-return-value 1738226435)
      (let* ((result (car (consult-hn--async-transform coll)))
             (comment-match (string-match "This is a comment" result)))
        (expect comment-match :not :to-be nil)
        (when comment-match
          (expect (get-text-property comment-match 'invisible result) :to-be t)))))

  (it "carries the rendered annotation on the candidate"
    (let ((coll (list (propertize "Title"
                                  'author "user"
                                  'ts 1738226435
                                  'created-at "2025-01-30T10:00:00"
                                  'comment "This is a comment"))))
      (spy-on 'float-time :and-return-value 1738226435)
      (let ((result (car (consult-hn--async-transform coll))))
        (expect (consult-hn--annotate result) :to-match "This is a comment")
        (expect (consult-hn--annotate result) :to-match "\\`\n"))))

  (it "annotates comment-free candidates with nothing"
    (let ((coll (list (propertize "Title"
                                  'author "user"
                                  'ts 1738226435
                                  'created-at "2025-01-30T10:00:00"
                                  'comment nil))))
      (spy-on 'float-time :and-return-value 1738226435)
      (expect (consult-hn--annotate (car (consult-hn--async-transform coll)))
              :to-equal ""))))

(describe "consult-hn--comment-annotation"
  (let ((long (mapconcat #'identity (make-list 600 "word") " ")))

    (it "caps at consult-hn-max-comment-lines with an ellipsis"
      (let ((consult-hn-max-comment-lines 2))
        (let ((lines (split-string (consult-hn--comment-annotation long) "\n")))
          (expect (length lines) :to-equal 2)
          (expect (car (last lines)) :to-match "…\\'"))))

    (it "honours a wider line budget"
      (let ((consult-hn-max-comment-lines 5))
        (expect (length (split-string (consult-hn--comment-annotation long) "\n"))
                :to-equal 5)))

    (it "leaves a short comment uncut"
      (let ((out (consult-hn--comment-annotation "short one")))
        (expect out :to-equal "  short one")
        (expect out :not :to-match "…")))

    (it "indents every shown line"
      (let ((consult-hn-max-comment-lines 3))
        (dolist (l (split-string (consult-hn--comment-annotation long) "\n"))
          (expect l :to-match "\\`  "))))

    (it "fills only what can be shown, regardless of comment size"
      ;; the guard against the cost growing with comment length: a
      ;; comment two orders of magnitude longer must not cost more
      (let* ((huge (mapconcat #'identity (make-list 60000 "word") " "))
             (t0 (float-time))
             (_ (consult-hn--comment-annotation huge))
             (huge-ms (- (float-time) t0))
             (t1 (float-time))
             (_ (consult-hn--comment-annotation long))
             (long-ms (- (float-time) t1)))
        (expect huge-ms :to-be-less-than (max 0.05 (* 20 long-ms)))))

    (it "returns nil for absent or blank comments"
      (expect (consult-hn--comment-annotation nil) :to-be nil)
      (expect (consult-hn--comment-annotation "") :to-be nil)
      (expect (consult-hn--comment-annotation "   ") :to-be nil))))

(describe "consult-hn--scale-vertico-count"
  ;; vertico is absent from the unit sandbox, so `vertico-count' starts
  ;; unbound in every spec here
  (it "no-ops when vertico is absent"
    (with-temp-buffer
      (consult-hn--scale-vertico-count)
      (expect (local-variable-p 'vertico-count) :to-be nil)))

  (it "divides the count by the candidate's line footprint"
    (unwind-protect
        (progn
          (setq vertico-count 15)
          (with-temp-buffer
            (let ((consult-hn-max-comment-lines 2))
              (consult-hn--scale-vertico-count)
              (expect (local-variable-p 'vertico-count) :to-be t)
              (expect vertico-count :to-equal 5))))
      (makunbound 'vertico-count)))

  (it "keeps a floor of four candidates"
    (unwind-protect
        (progn
          (setq vertico-count 9)
          (with-temp-buffer
            (let ((consult-hn-max-comment-lines 3))
              (consult-hn--scale-vertico-count)
              (expect vertico-count :to-equal 4))))
      (makunbound 'vertico-count)))

  (it "respects a count already made buffer-local"
    (unwind-protect
        (progn
          (setq vertico-count 15)
          (with-temp-buffer
            (setq-local vertico-count 42)
            (consult-hn--scale-vertico-count)
            (expect vertico-count :to-equal 42)))
      (makunbound 'vertico-count))))

(describe "consult-hn--process-results"
  (it "processes API results correctly"
    (let* ((hit1 (make-hash-table :test 'equal))
           (hit2 (make-hash-table :test 'equal))
           (result (make-hash-table :test 'equal)))
      ;; Setup hit1
      (puthash "author" "test_user" hit1)
      (puthash "title" "Test Title" hit1)
      (puthash "url" "https://example.com" hit1)
      (puthash "created_at" "2025-01-30T10:00:00" hit1)
      (puthash "created_at_i" 1738226435 hit1)
      (puthash "objectID" "12345" hit1)
      (puthash "story_id" "12345" hit1)
      (puthash "points" 42 hit1)
      (puthash "num_comments" 10 hit1)
      
      ;; Setup hit2 (comment)
      (puthash "author" "comment_user" hit2)
      (puthash "story_title" "Story Title" hit2)
      (puthash "comment_text" "<p>This is a comment</p>" hit2)
      (puthash "created_at" "2025-01-30T11:00:00" hit2)
      (puthash "created_at_i" 1738230035 hit2)
      (puthash "objectID" "67890" hit2)
      (puthash "story_id" "12345" hit2)
      
      ;; Setup result
      (puthash "hits" (list hit1 hit2) result)
      
      (let ((processed (consult-hn--process-results result)))
        (expect (length processed) :to-equal 2)
        
        ;; Check first item (story)
        (let ((item1 (car processed)))
          (expect (get-text-property 0 'object-id item1) :to-equal "12345")
          (expect (get-text-property 0 'title item1) :to-equal "Test Title")
          (expect (get-text-property 0 'author item1) :to-equal "test_user")
          (expect (get-text-property 0 'story-url item1) :to-equal "https://example.com")
          (expect (get-text-property 0 'points item1) :to-equal 42)
          (expect (get-text-property 0 'num-comments item1) :to-equal 10))
        
        ;; Check second item (comment)
        (let ((item2 (cadr processed)))
          (expect (get-text-property 0 'title item2) :to-equal "Story Title")
          (expect (get-text-property 0 'author item2) :to-equal "comment_user")
          (expect (get-text-property 0 'comment item2) :to-match "This is a comment")))))

  (it "handles missing fields gracefully"
    (let* ((hit (make-hash-table :test 'equal))
           (result (make-hash-table :test 'equal)))
      ;; Minimal hit
      (puthash "author" "user" hit)
      (puthash "created_at" "2025-01-30T10:00:00" hit)
      (puthash "created_at_i" 1738226435 hit)
      (puthash "objectID" "123" hit)
      (puthash "story_id" "123" hit)
      
      (puthash "hits" (list hit) result)
      
      (let ((processed (consult-hn--process-results result)))
        (expect (length processed) :to-equal 1)
        (expect processed :not :to-throw)))))

(describe "consult-hn--async-lookup"
  (it "finds matching candidate"
    (let* ((coll (list (propertize "Test Title"
                                   'title "Test Title Full"
                                   'created-at "2025-01-30T10:00:00")))
           (cand "Test Title   user   2 hours ago   2025-01-30T10:00:00"))
      (expect (consult-hn--async-lookup cand coll nil nil)
              :to-equal (car coll))))

  (it "returns nil when no match found"
    (let* ((coll (list (propertize "Different Title"
                                   'title "Different Title"
                                   'created-at "2025-01-30T10:00:00")))
           (cand "Test Title   user   2 hours ago   2025-01-30T11:00:00"))
      (expect (consult-hn--async-lookup cand coll nil nil)
              :to-be nil)))

  (it "handles truncated titles with ellipsis"
    (let* ((coll (list (propertize "Very Long Title That Gets Truncated"
                                   'title "Very Long Title That Gets Truncated"
                                   'created-at "2025-01-30T10:00:00")))
           (cand "Very Long Title That Gets Trun...   user   2 hours ago   2025-01-30T10:00:00"))
      (expect (consult-hn--async-lookup cand coll nil nil)
              :to-equal (car coll))))

  (it "handles nil inputs gracefully"
    (expect (consult-hn--async-lookup nil '() nil nil) :to-be nil)
    (expect (consult-hn--async-lookup "cand" nil nil nil) :to-be nil)))

;; `defvar' without a value only marks a variable special in the file it
;; appears in, so the package's own declarations do not reach here
(defvar embark-general-map)
(defvar embark-keymap-alist)
(defvar embark-default-action-overrides)

(describe "embark actions"
  (let ((item (propertize "Test Title"
                          'title "Test Title"
                          'author "someone"
                          'story-url "https://example.com/article"
                          'hn-story-url "https://news.ycombinator.com/item?id=1"
                          'hn-object-url "https://news.ycombinator.com/item?id=2")))

    (it "hands the whole object to `consult-hn-browse-fn'"
      (let (seen)
        (let ((consult-hn-browse-fn (lambda (&rest args) (setq seen args))))
          (consult-hn--open item))
        (expect (plist-get seen :title) :to-equal "Test Title")
        (expect (plist-get seen :hn-object-url)
                :to-equal "https://news.ycombinator.com/item?id=2")))

    (it "browses the object, not the story it points at"
      (spy-on 'browse-url)
      (consult-hn--browse-url item)
      (expect 'browse-url :to-have-been-called-with
              "https://news.ycombinator.com/item?id=2"))

    (it "opens the object in eww"
      (spy-on 'consult-hn-eww)
      (consult-hn--browse-eww item)
      (expect (plist-get (spy-calls-args-for 'consult-hn-eww 0) :hn-object-url)
              :to-equal "https://news.ycombinator.com/item?id=2"))

    (it "copies the object url"
      (let ((kill-ring nil)
            (kill-ring-yank-pointer nil)
            (interprogram-cut-function nil))
        (consult-hn--copy-url item)
        (expect (current-kill 0) :to-equal "https://news.ycombinator.com/item?id=2")))

    (it "keeps every browse key on one prefix"
      (expect (lookup-key consult-hn-embark-map (kbd "b b")) :to-be #'consult-hn--open)
      (expect (lookup-key consult-hn-embark-map (kbd "b o")) :to-be #'consult-hn--browse-url)
      (expect (lookup-key consult-hn-embark-map (kbd "b e")) :to-be #'consult-hn--browse-eww)
      (expect (lookup-key consult-hn-embark-map (kbd "w")) :to-be #'consult-hn--copy-url))

    (it "registers the category with embark"
      ;; embark is not a test dependency, so stand in for what it defines
      (let ((embark-general-map (make-sparse-keymap))
            (embark-keymap-alist nil)
            (embark-default-action-overrides nil)
            (parent (keymap-parent consult-hn-embark-map)))
        (unwind-protect
            (progn
              (consult-hn--embark-setup)
              (expect (alist-get 'consult-hn-result embark-keymap-alist)
                      :to-be 'consult-hn-embark-map)
              (expect (alist-get 'consult-hn-result embark-default-action-overrides)
                      :to-be #'consult-hn--open)
              (expect (keymap-parent consult-hn-embark-map) :to-be embark-general-map))
          (set-keymap-parent consult-hn-embark-map parent))))))

;;; consult-hn-tests.el ends here
