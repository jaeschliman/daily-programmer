(defpackage :challenge-20150916 (:use :cl :alexandria))
(in-package :challenge-20150916)
;; https://www.reddit.com/r/dailyprogrammer/comments/3l61vx/20150916_challenge_232_intermediate_where_should/ 

(defun distsq (pairs)
  (destructuring-bind ((x1 . y1) (x2 . y2)) (coerce pairs 'list)
    (let ((a (- x1 x2))
          (b (- y1 y2)))
      (+ (* a a) (* b b)))))

(defun read-pair (line)
  (let* ((*read-default-float-format* 'double-float)
         (split (position #\, line))
         (close (position #\) line))
         (a (read-from-string line nil nil :start 1 :end split))
         (b (read-from-string line nil nil :start (1+ split) :end close)))
     (cons a b)))

(defun print-pair (pair &optional (stream *standard-output*))
  (format stream "(~f,~f)" (car pair) (cdr pair)))

(defun print-pairs (pairs &optional (stream *standard-output*))
  (print-pair (elt pairs 0) stream)
  (write-char #\Space stream)
  (print-pair (elt pairs 1) stream))

(defun read-problem (pathname)
  (with-open-file (s pathname :direction :input)
    (let ((count (parse-integer (read-line s) :junk-allowed t)))
      (loop repeat count collect (read-pair (read-line s))))))

(defun naiive (pairs)
  (let ((dist most-positive-double-float)
        (best nil))
    (map-combinations
     (lambda (pair)
       (let ((ds (distsq pair)))
         (when (< ds dist)
           (setf dist ds
                 best pair))))
     pairs
     :length 2)
    (cons best dist)))

(defun min-by (key &rest list)
  (when list
    (let ((result (car list)) (best (funcall key (car list))))
      (dolist (item (rest list))
        (let ((score (funcall key item)))
          (when (< score best)
            (setf result item best score))))
      result)))

(defun divide-and-conquer (pairs)
  "port of /u/eatsfrombags python implementation"
  (setf pairs (coerce pairs 'vector))
  (sort pairs '< :key #'car)
  (labels
      ((closest (pairs)
         (if (<= (length pairs) 3)
             (naiive pairs)
             (let* ((mid          (floor (length pairs) 2))
                    (midpoint     (elt pairs mid))
                    (left         (closest (subseq pairs 0 mid)))
                    (right        (closest (subseq pairs mid)))
                    (min          (min-by #'cdr left right))
                    (min-dist     (cdr min))
                    (close-to-mid (lambda (p) (< (abs (- (car p) (car midpoint))) min-dist)))
                    (splits       (remove-if-not close-to-mid pairs)))
               (if (> (length splits) 1)
                   (min-by #'cdr (naiive splits) min)
                   min)))))
    (closest pairs)))

(defun solve-file (pathname)
  (let* ((input (read-problem pathname))
         (solution (divide-and-conquer input)))
    (print-pairs (car solution))))
