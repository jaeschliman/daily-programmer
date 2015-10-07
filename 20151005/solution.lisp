(ql:quickload :alexandria)
(defpackage :challenge-20151005 (:use :cl :alexandria))
(in-package :challenge-20151005)
;;https://www.reddit.com/r/dailyprogrammer/comments/3nkanm/20151005_challenge_235_easy_ruthaaron_pairs/

(defvar *tuple-readtable* (copy-readtable))
(set-syntax-from-char #\, #\Space *tuple-readtable*)

(defun read-tuple (stream)
  (let ((*readtable* *tuple-readtable*))
    (read stream)))

(defun read-problem (pathname)
  (with-input-from-file (s pathname)
    (loop repeat (read s) collect (read-tuple s))))

(defun sum-of-prime-factors (n)
  (prog ((sum 0) (i 2))
     :start
     (when (= 0 (mod n i))
       (incf sum i)
       (loop while (= 0 (mod n i))
            do (setf n (/ n i))))
     (incf i)
     (unless (> i n) (go :start))
     (return sum)))

(defun solve-tuple (tuple &optional (stream *standard-output*))
  (let* ((valid? (= (sum-of-prime-factors (first tuple))
                    (sum-of-prime-factors (second tuple))))
         (str (if valid? "VALID" "NOT VALID")))
    (format stream "(~{~A, ~A~}) ~A~%" tuple str)))

(defun solve-file (pathname)
  (let ((tuples (read-problem pathname)))
    (map nil 'solve-tuple tuples)))
