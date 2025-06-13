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
  (it "bogus keys get removed"
    (let ((consult-hn-default-search-params '((hitsPerPage 125))))
      (expect (consult-hn--input->params "foo -- zop=120")
              :to-equal '((query "foo") (hitsPerPage 125))))))

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
          (expect (get-text-property comment-match 'invisible result) :to-be t))))))

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
