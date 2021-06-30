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

(defmacro make-get-call (&body body)
  `(multiple-value-bind (body status headers uri connection)
      (dexador:get
        ,@body
        :headers (list 
                  (cons "Authorization" (format nil "token ~A" *apitoken*))))
    (jsown:parse body)))

(defmacro make-post-call (url &body body)
  `(multiple-value-bind (body status headers uri connection)
      (dexador:post
        ,url
        :headers (list 
                  (cons "Authorization" (format nil "token ~A" *apitoken*))
                  (cons "Content-Type" "application-json"))
        ,@body)
      status))


(defun get-repo-details (repo)
  (make-get-call (concatenate 'string *api-url* "repos/" *username* "/" repo)))

(defun get-repos ()
  (make-get-call (concatenate 'string *api-url* (format nil "users/~A/repos" *username*))))

(defun create-repo (repo)
  (make-post-call
        (concatenate 'string *api-url* "user/repos")
        :content (cl-json:encode-json-alist-to-string
                   `(("name" . ,repo)
                     ("auto_init" . "true")))))
    ; (alexandria:hash-table-alist headers)))

    ; (jsown:parse body)))

  
(print (get-repos))
; (when (= (create-repo "notes") 201)
;   (print (get-repo-details "notes")))
