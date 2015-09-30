(defpackage :challenge-20150928 (:use :cl :alexandria))
(in-package :challenge-20150928)

(defun map-number-product (fn lower upper &key (length 1))
  (labels ((%map-product (acc count)
             (loop for i from lower to upper
                for is = (cons i acc) do
                  (if (zerop count)
                      (funcall fn is)
                      (%map-product is (1- count))))))
    (%map-product nil (1- length))))

(defun digit-length (number)
  (do ((n number (floor n 10))
       (count 0 (1+ count)))
      ((zerop n) count)))

(defun digits-of (number)
  (let (result)
    (loop until (zerop number) do 
         (multiple-value-bind (remaining digit) (floor number 10)
           (setf number remaining)
           (push digit result)))
    result))

(defun vampire? (number fangs expected-length)
  (when (= (digit-length number) expected-length)
    (equal (sort (digits-of number) '<)
           (sort (reduce 'append (mapcar 'digits-of fangs)) '<))))

(defun print-vampire-numbers (expected-length fang-count)
  (let ((fang-digit-count (/ expected-length fang-count))
        (seen (make-hash-table :test 'equal)))
    (map-number-product (lambda (fangs)
                          (let ((number (reduce '* fangs)))
                            (when (vampire? number fangs expected-length)
                              (let ((key (sort (copy-list fangs) '<)))
                                (unless #1=(gethash key seen)
                                  (setf #1# t)
                                  (format t "~A=~{~A~^*~}~%" number fangs))))))
                        (expt 10 (1- fang-digit-count))
                        (1- (expt 10 fang-digit-count))
                        :length fang-count)))
