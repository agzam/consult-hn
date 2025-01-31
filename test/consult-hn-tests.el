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

;;; consult-hn-tests.el ends here
