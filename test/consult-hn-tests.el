;;; consult-hn-tests.el --- tests for consult-hn.el -*- lexical-binding: t; -*-
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

(describe "consult-hn--plist->prop-string"
  (it "converts basic plist to propertized string"
    (let ((result (consult-hn--plist->prop-string
                   '(:title "Hello" :url "http://example.com") :title)))
      (expect (get-text-property 0 'url result) :to-equal "http://example.com")
      (expect result :to-equal "Hello")))

  (it "handles empty properties"
    (let ((result (consult-hn--plist->prop-string '(:title "Test" :url nil) :title)))
      (expect result :to-equal "Test")
      (expect (get-text-property 0 'url result) :to-be nil)))

  (it "preserves multiple properties"
    (let ((result (consult-hn--plist->prop-string
                   '(:text "Sample" :url "http://test.com" :id 123 :score 45)
                   :text)))
      (expect result :to-equal "Sample")
      (expect (get-text-property 0 'url result) :to-equal "http://test.com")
      (expect (get-text-property 0 'id result) :to-equal 123)
      (expect (get-text-property 0 'score result) :to-equal 45))))

(describe "consult-hn--normalize-input"
  (it "handles nil and blank strings"
    (expect (consult-hn--normalize-input nil) :to-equal nil)
    (expect (consult-hn--normalize-input "") :to-equal nil))
  (it "basic input gets turn into query"
    (let ((consult-hn-default-search-params nil))
      (expect (consult-hn--normalize-input "foo") :to-equal "query=foo")))
  (it "basic input with separator but no additional params"
    (let ((consult-hn-default-search-params nil))
      (expect (consult-hn--normalize-input "foo --") :to-equal "query=foo")))
  (it "multiple tags properly parse"
    (let ((consult-hn-default-search-params nil))
      (expect (consult-hn--normalize-input "foo -- tags=(story,author_boo)") :to-equal "query=foo&tags=(story,author_boo)")))
  (it "default params accounted for"
    (let ((consult-hn-default-search-params '((hitsPerPage 125) (tags "comment"))))
      (expect (consult-hn--normalize-input "foo --") :to-equal "query=foo&hitsPerPage=125&tags=comment")))
  (it "bogus keys get removed"
    (let ((consult-hn-default-search-params '((hitsPerPage 125))))
      (expect (consult-hn--normalize-input "foo -- zop=120") :to-equal "query=foo&hitsPerPage=125"))))

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

;;; consult-hn-tests.el ends here
