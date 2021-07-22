(defpackage cl-github
  (:use :cl)
  (:export
    :main))
(in-package :cl-github)

(defparameter *tokens-file* "tokens.lisp")
(defparameter *username* nil)
(defparameter *apitoken* nil)
(defparameter *api-url* "https://api.github.com")



(defun load-tokens (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *username* (read in))
      (setf *apitoken* (read in)))))


(defmacro get-url (url)
  `(multiple-value-bind (body status headers uri connection)
      (dexador:get
        ,url
        :headers (list 
                  (cons "Authorization" (format nil "token ~A" *apitoken*))
                  (cons "Content-Type" "application-json")))
    (values status body)))

(defmacro post-url (url &body body)
  `(multiple-value-bind (body status headers uri connection)
      (dexador:post
        ,url
        :headers (list 
                  (cons "Authorization" (format nil "token ~A" *apitoken*))
                  (cons "Content-Type" "application-json"))
        ,@body)
      (values status body)))


(defun get-repo-details (repo)
  (let ((url (format nil "~A/repos/~A/~A" *api-url* *username* repo)))
    (get-url url)))

(defun get-repos ()
  (let ((url (format nil "~A/users/~A/repos" *api-url* *username*)))
    (get-url url)))

(defun create-repo (repo &key (private t))
  (post-url
        (format nil "~A/~A" *api-url* "user/repos")
        :content (cl-json:encode-json-alist-to-string
                   `(("name" . ,repo)
                     ("auto_init" . "true")
                     ("private" . ,private)))))
    ; (alexandria:hash-table-alist headers)))

    ; (jsown:parse body)))

(defmain:defmain (main :PROGRAM-NAME "github")
    ((debug "Show traceback instead of short message."
            :flag t)
     (log   "Filename to write log to.")
     (token "GitHub personal access token."
            :env-var "TOKEN")
     &subcommand)
  "Utility to analyze github forks.")
  ; (defmain:subcommand)
  
(load-tokens *tokens-file*)

(defmain:defcommand (main create)
  ((private "Create Private repository"
            :flag t)
   (repository "Name of the repository to create.")
   &rest repository)
  "Create a new repository on Github"
  (format t "Repository ~A~%" repository))
  

(defmain:defcommand (main info)
  ((user "Username")
   (repository "Name repository"))
  "Get details of a repository on Github"
  (let ((*username* user))
    (get-repo-details repository)))




; (defmain:defcommand (main info))

; (main)
