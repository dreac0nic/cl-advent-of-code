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


(defun get-puzzle-input (year day &optional token)
  "Retrieve the user's pauzzle input for the year/day specified."
  (nth-value 0
             (dexador:get (format nil "~a/input" (build-endpoint year day))
                          :cookie-jar (bake-cookies token))))


(defun submit-answer (year day answer &key token (part :part-one))
  "Submits the given answer for the day specified. Optionally :part-one and :part-two can be specified."
  (elt (lquery:$ (initialize (dexador:post (format nil "~a/answer" (build-endpoint year day))
                                           :cookie-jar (bake-cookies token)
                                           :content `(("level" . ,(format nil "~a" (case part
                                                                                     (:part-one 1)
                                                                                     (:part-two 2)
                                                                                     (t 1))))
                                                      ("answer" . ,(format nil "~a" answer)))))
         "body main article p"
         (render-text))
       0))


(defmacro with-puzzle ((&key day token year)
                       (&key input-binding)
                       &body body)
  "Provides a framework for interacting with Advent of Code, including pulling the puzzle input and submitting answers."
  (let ((G!day (gensym "DAY"))
        (G!year (gensym "YEAR"))
        (G!token (gensym "TOKEN")))
    `(let* ((,G!day ,day)
            (,G!year ,year)
            (,G!token ,token)
            (,input-binding (get-puzzle-input ,G!year ,G!day ,G!token)))
       (labels ((submit-part-one (answer)
                  (submit-answer ,G!year ,G!day answer :token ,G!token :part :part-one))
                (submit-part-two (answer)
                  (submit-answer ,G!year ,G!day answer :token ,G!token :part :part-two)))
         (declare (ignorable (function submit-part-one)
                             (function submit-part-two)))
         (progn
           ,@body)))))
