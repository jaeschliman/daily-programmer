(ql:quickload :alexandria)
(defpackage :challenge-20150909 (:use :cl :alexandria))
(in-package :challenge-20150909)

(defun all-same-or-different (a b c)
  (or (and (equal a b) (equal b c))
      (not (or (equal a b)
               (equal b c)
               (equal c a)))))

(defun is-hand? (cards)
  (apply 'every 'all-same-or-different cards))

(defun find-hands (list)
  (let (result)
    (map-combinations
     (lambda (cards)
       (when (is-hand? cards)
         (push cards result)))
     list :length 3)
    (reverse result)))

(defun read-problem (pathname)
  (with-input-from-file (s pathname)
    (loop repeat 12 collect (read-line s nil nil))))

(defun solve-file (pathname)
  (let* ((cards (read-problem pathname))
         (hands (find-hands cards)))
    (format t "~{~{~A~^ ~}~%~}" hands)))
