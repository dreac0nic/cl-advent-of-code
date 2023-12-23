;;;; cl-advent-of-code.lisp

(in-package #:cl-advent-of-code)


(defvar *protocol* "https")
(defvar *domain* "adventofcode.com")


(defun slurp (path)
  "Reads in entire file specified by path as a string"
  (with-open-file (stream path)
    (let ((buffer (make-string (file-length stream))))
      (read-sequence buffer stream)
      (string-trim '(#\Space #\Newline #\Tab) buffer))))


(defun build-endpoint (year day)
  "Build the endpoint for a specific day's API."
  (format nil "~a://~a/~d/day/~d"
          *protocol*
          *domain*
          year
          day))


(defun bake-cookies (token)
  "Creates a cookie jar with the token packed as a session cookie"
  (let ((cookies (make-cookie-jar)))
    (-<>> (or token
              (slurp ".session")
              (sb-ext:posix-getenv "AOC_SESSION"))
          (make-cookie :name "session"
                       :value <>
                       :domain (format nil ".~a" *domain*)
                       :secure-p 't
                       :httponly-p 't
                       :path "/")
          list
          (merge-cookies cookies))
    cookies))
