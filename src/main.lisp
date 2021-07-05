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


(defmacro make-get-call (url)
  `(multiple-value-bind (body status headers uri connection)
      (dexador:get
        ,url
        :headers (list 
                  (cons "Authorization" (format nil "token ~A" *apitoken*))
                  (cons "Content-Type" "application-json")))
    (values status body)))

(defmacro make-post-call (url &body body)
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
    (make-get-call url)))

(defun get-repos ()
  (let ((url (format nil "~A/users/~A/repos" *api-url* *username*)))
    (make-get-call url)))

(defun create-repo (repo &key (private t))
  (make-post-call
        (format nil "~A/~A" *api-url* "user/repos")
        :content (cl-json:encode-json-alist-to-string
                   `(("name" . ,repo)
                     ("auto_init" . "true")
                     ("private" . ,private)))))
    ; (alexandria:hash-table-alist headers)))

    ; (jsown:parse body)))

(opts:define-opts
   (:name :help
          :description "print this help text"
          :short #\h
          :long "help")
   (:name :nb
          :description "here we want a number argument"
          :short #\n
          :long "nb"
          :arg-parser #'parse-integer) ;; <- takes an argument
   (:name :info
          :description "info"
          :short #\i
          :long "info"))

  
(defun unknown-option (condition)
  (format t "warning: ~s option is unknown!~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(defun main ()
  (multiple-value-bind (options free-args)
      (handler-case
          (handler-bind ((opts:unknown-option #'unknown-option))
            (opts:get-opts))
        (opts:missing-arg (condition)
          (format t "fatal: option ~s needs an argument!~%"
                  (opts:option condition)))
        (opts:arg-parser-failed (condition)
          (format t "fatal: cannot parse ~s as argument of ~s~%"
                  (opts:raw-arg condition)
                  (opts:option condition)))
        (opts:missing-required-option (con)
          (format t "fatal: ~a~%" con)
          (opts:exit 1)))
    ;; Here all options are checked independently, it's trivial to code any
    ;; logic to process them.
    (when-option (options :help)
      (opts:describe)
      :prefix "example—program to demonstrate unix-opts library"
      :suffix "so that's how it works…"
      :usage-of "example.sh"
      :args     "[FREE-ARGS]")
    (when-option (options :verbose)
      (format t "OK, running in verbose mode…~%"))
    (when-option (options :level)
      (format t "I see you've supplied level option, you want ~a level!~%" it))
    (when-option (options :output)
      (format t "I see you want to output the stuff to ~s!~%"
              (getf options :output)))
    (format t "free args: ~{~a~^, ~}~%" free-args)))
