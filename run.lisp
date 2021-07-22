(load "cl-github.asd")

(ql:quickload :cl-github)

(in-package :cl-github)

; (load-tokens "tokens.lisp")

(load-tokens *tokens-file*)
; (print (get-repo-details "cl-duikboot"))
; (print (get-repos))
; (when (= (create-repo "notes") 201)
; (print (jsown:parse (get-repo-details "notes") "ssh_url"))

; (multiple-value-bind (status json) (get-repo-details "notes")
;   (print (jsown:parse json "summary" "ssh_url" "private")))

; (multiple-value-bind (status json) (get-repo-details "notes")
;   (declare (ignorable status json))
;   (print (jsown:keywords (jsown:parse json))))
(main)
