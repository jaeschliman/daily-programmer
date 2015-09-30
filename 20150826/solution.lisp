(ql:quickload '(:screamer :alexandria))
(screamer:define-screamer-package :challenge-20150826
  (:use :alexandria))
(in-package :challenge-20150826)
;; https://www.reddit.com/r/dailyprogrammer/comments/3iimw3/20150826_challenge_229_intermediate_reverse_fizz/

(defparameter *upper-bound* 5000)

(defun make-var (char)
  (an-integer-betweenv 1 *upper-bound* (princ-to-string char)))

(defun get-var (char vars)
  (if-let (v #1=(gethash char vars)) v
    (setf #1# (make-var char))))

(defun increase-multiple (var multiples)
  (incf (gethash var multiples 0)))

(defun equal-multiples (a b multiples)
  (let ((am (gethash a multiples))
        (bm (gethash b multiples)))
    (assert! (=v (*v am a) (*v bm b)))))

(defun process-line (line variables multiples)
  (let ((vars (map 'list (lambda (char) (get-var char variables)) line)))
    (dolist (v vars) (increase-multiple v multiples))
    (when (> (length vars) 1)
      (map-combinations
       (lambda (vars) (equal-multiples (elt vars 0) (elt vars 1) multiples))
       vars :length 2 :copy nil))
    (dolist (a vars)
      (let ((am (gethash a multiples)))
        (maphash (lambda (b bm)
                   (unless (or (eq a b) (member b vars))
                     (assert! (>v (*v am a) (*v bm b)))))
                 multiples)))))

(defun add-missing-variables (variables multiples)
  (labels ((missing-vars ()
             (loop
                with var-names = (hash-table-keys variables)
                with max = (reduce 'max var-names :key 'char-code)
                with min = (char-code #\a)  
                for code from min to max for name = (code-char code)
                unless (gethash name variables) collect
                  (cons name (make-var name)))))
    (let ((added (missing-vars))) 
      (loop for (char . var) in added do
           (maphash (lambda (b bm)
                      (assert! (>v var (*v bm b))))
                    multiples))
      (loop for (char . var) in added do
           (setf (gethash char variables) var
                 (gethash var  multiples) 1)))))

(defun solve-lines (lines)
  (let ((variables (make-hash-table))
        (multiples (make-hash-table)))
    (dolist (line lines)
      (process-line line variables multiples))
    (add-missing-variables variables multiples)
    (let* ((vars (hash-table-alist variables)))
      (setf vars (sort vars 'char< :key #'car))
      (let* ((ordering (static-ordering #'divide-and-conquer-force))
             (solution (one-value (solution (mapcar 'cdr vars) ordering)))
             (mapping  (map 'list (lambda (var value) (list (car var) value))
                            vars solution)))
        (format t "~{~{~C=~A ~}~}" mapping)))))

(defun read-problem (pathname)
  (with-input-from-file (s pathname)
    (loop for line = (read-line s nil nil) while line collect line)))

(defun solve-file (pathname)
  (solve-lines (read-problem pathname)))
