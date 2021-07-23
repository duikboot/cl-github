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

(defun get-method (method)
  (case method
    (get #'dexador:get)
    (post #'dexador:post)))

(defun content (method)
  (when (eq method post)))

(defmacro call (method url &body body)
  `(multiple-value-bind (json status headers uri connection)
      (if (eq ,method 'get)
        (dexador:get
          ,url
          :headers (list
                    (cons "Authorization" (format nil "token ~A" *apitoken*))
                    (cons "Content-Type" "application-json")))
        (dexador:post
          ,url
          :headers (list
                    (cons "Authorization" (format nil "token ~A" *apitoken*))
                    (cons "Content-Type" "application-json"))
          :content ,@body))
    (values status json)))

(defmacro post-url (url &body body)
  `(multiple-value-bind (body status headers uri connection)
      (dexador:post
        ,url
        :headers (list
                  (cons "Authorization" (format nil "token ~A" *apitoken*))
                  (cons "Content-Type" "application-json"))
        :content ,@body)
      (values status body)))


(defun get-repo-details (repo)
  (let ((url (format nil "~A/repos/~A/~A" *api-url* *username* repo)))
    (multiple-value-bind (status json) (call 'get url nil)
      (jsown:parse json "owner" "full_name" "ssh_url" "private"))))

(defun get-repos ()
  (let ((url (format nil "~A/users/~A/repos" *api-url* *username*)))
    (call 'get url)))

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
    ((username "Owner of the repository")
     (token "GitHub personal access token."
            :env-var "TOKEN")
     (debug "Show traceback instead of short message."
            :flag t)
     (log   "Filename to write log to.")
     &subcommand)
  "Utility to analyze github forks.")
  ; (defmain:subcommand)

(load-tokens *tokens-file*)

(defmain:defcommand (main repo)
  ((repository "Name of the repository")
   &subcommand)
  "Repository")

; (defmain:defcommand (repo create)
;   ((private "Create Private repository"
;             :flag t)
;    (repository "Name of the repository to create.")
;    &rest repository)
;   "Create a new repository on Github"
;   (format t "Repository ~A~%" repository))


(defmain:defcommand (repo info)
  ; ((repository "name of the repo"))
  (&rest repository)
  "Get info on repository."
  (format t "~A~%" (get-repo-details (first repository))))

; (defmain:defcommand (repo info)
;   ((user "Username")
;    (repository "Name repository"))
;   "Get details of a repository on Github"
;   (let ((*username* user))
;     (get-repo-details repository)))

; (defmain:defcommand (main info))

; (main)
