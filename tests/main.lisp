(defpackage cl-github/tests/main
  (:use :cl
        :cl-github
        :rove))
(in-package :cl-github/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-github)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
