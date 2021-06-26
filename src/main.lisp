(defpackage cl-github
  (:use :cl)
  (:export
    :main))
(in-package :cl-github)

(defparameter *tokens-file* "tokens.lisp")
(defparameter *username* nil)
(defparameter *apitoken* nil)
(defparameter *api-url* "https://api.github.com/users/~A")
(defparameter *api-url* (format nil "https://api.github.com/users/~A/repos" *username*))
(defparameter *create-api-url* (format nil "https://api.github.com/user/repos")) 



(defun load-tokens (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *username* (read in))
      (setf *apitoken* (read in)))))

(load-tokens *tokens-file*)

(defun get-repos ()
  (multiple-value-bind (body status headers uri connection)
      (dexador:get
        *api-url*
        :headers (list 
                   (cons "Authorization" (format nil "token ~A" *apitoken*))))
    (jsown:parse body)))
  
(defun create-repo (repo)
  (multiple-value-bind (body status headers uri connection)
      (dexador:post
        *create-api-url*
        :headers (list 
                   (cons "Authorization" (format nil "token ~A" *apitoken*))
                   (cons "Content-Type" "application-json"))
        :content (cl-json:encode-json-alist-to-string
                   `(("name" . ,repo)
                     ("auto_init" . "true")))
        :verbose t)
    (alexandria:hash-table-alist headers)))
    ; (jsown:parse body)))
  
(print (get-repos))
(create-repo "cl-github")
