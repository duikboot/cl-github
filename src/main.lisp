(defpackage cl-github
  (:use :cl)
  (:export
    :main))
(in-package :cl-github)

(defparameter *tokens-file* "tokens.lisp")
(defparameter *username* nil)
(defparameter *apitoken* nil)
(defparameter *api-url* "https://api.github.com/")



(defun load-tokens (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *username* (read in))
      (setf *apitoken* (read in)))))

(load-tokens *tokens-file*)

(defun get-repo-details (repo)
  (let ((*api-url* (concatenate 'string *api-url* "repos/" *username* "/" repo)))
    (multiple-value-bind (body status headers uri connection)
        (dexador:get
          *api-url*
          :headers (list 
                    (cons "Authorization" (format nil "token ~A" *apitoken*))))
      (jsown:parse body))))


(defun get-repos ()
  (multiple-value-bind (body status headers uri connection)
      (dexador:get
        (concatenate 'string *api-url* (format nil "users/~A/repos" *username*))
        :headers (list 
                   (cons "Authorization" (format nil "token ~A" *apitoken*))))
    (jsown:parse body)))
  
(defun create-repo (repo)
  (multiple-value-bind (body status headers uri connection)
      (dexador:post
        (concatenate 'string *api-url* "user/repos")
        :headers (list 
                   (cons "Authorization" (format nil "token ~A" *apitoken*))
                   (cons "Content-Type" "application-json"))
        :content (cl-json:encode-json-alist-to-string
                   `(("name" . ,repo)
                     ("auto_init" . "true")))
        :verbose t)
    (alexandria:hash-table-alist headers)))
    ; (jsown:parse body)))
  
(print (get-repo-details "cl-github"))
; (print (get-repos))
; (create-repo "cl-github")
