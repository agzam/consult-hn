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
                              (consult-hn-tests--params :type 'story)))
            :to-equal '("story"))
    (expect (alist-get 'tags (consult-hn--params-render
                              (consult-hn-tests--params :type 'comment)))
            :to-equal '("comment"))
    (expect (alist-get 'tags (consult-hn--params-render
                              (consult-hn-tests--params :type 'all)))
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
                              (consult-hn-tests--params
                               :type 'comment :author "pg" :front-page t)))
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
             (consult-hn-tests--params
              :query "emacs lisp" :type 'story :author "pg" :points 100
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
                              (consult-hn-tests--params :type 'story)))
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

(describe "consult-hn--input->params"
  (it "handles nil and blank strings"
    (expect (consult-hn--input->params nil) :to-equal nil)
    (expect (consult-hn--input->params "") :to-equal nil))
  (it "basic input gets turn into query"
    (let ((consult-hn-default-search-params nil))
      (expect (consult-hn--input->params "foo") :to-equal '((query "foo")))))
  (it "basic input with separator but no additional params"
    (let ((consult-hn-default-search-params nil))
      (expect (consult-hn--input->params "foo --") :to-equal '((query "foo")))))
  (it "multiple tags properly parse"
    (let ((consult-hn-default-search-params nil))
      (expect (consult-hn--input->params "foo -- tags=(story,author_boo)")
              :to-equal '((query "foo") (tags "(story,author_boo)")))
      (expect (consult-hn--input->params "foo -- tags=story,author_boo")
              :to-equal '((query "foo") (tags "story,author_boo")))))
  (it "default params accounted for"
    (let ((consult-hn-default-search-params '((hitsPerPage 125) (tags "comment"))))
      (expect (consult-hn--input->params "foo --")
              :to-equal '((query "foo") (hitsPerPage 125) (tags "comment")))))
  (it "empty query allowed"
    (let ((consult-hn-default-search-params nil))
      (expect (consult-hn--input->params "-- tags=front_page")
              :to-equal '((tags "front_page")))))
  (it "leaves the query for the query builder to encode"
    ;; a two-word query used to go out as query=a%2520b and match nothing
    (let ((consult-hn-default-search-params nil))
      (expect (consult-hn--input->params "elpaca emacs")
              :to-equal '((query "elpaca emacs")))
      (expect (url-build-query-string (consult-hn--input->params "elpaca emacs"))
              :to-equal "query=elpaca%20emacs")))

  (it "encodes query punctuation exactly once"
    (let ((consult-hn-default-search-params nil))
      (expect (url-build-query-string (consult-hn--input->params "c++ & rust"))
              :not :to-match "%25")))

  (it "bogus keys get removed"
    (let ((consult-hn-default-search-params '((hitsPerPage 125))))
      (expect (consult-hn--input->params "foo -- zop=120")
              :to-equal '((query "foo") (hitsPerPage 125))))))

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
      (expect (buffer-live-p buffer) :to-be nil))))

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

;;; consult-hn-tests.el ends here
