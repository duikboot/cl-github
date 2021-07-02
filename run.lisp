(load "cl-github.asd")

(ql:quickload :cl-github)

(in-package :cl-github)

; (print (get-repos))
(print (get-repo-details "cl-duikboot"))
; (when (= (create-repo "notes") 201)
;   (print (get-repo-details "notes")))
