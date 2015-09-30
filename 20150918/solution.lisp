(defpackage :challenge-20150918 (:use :cl :alexandria))
(in-package :challenge-20150918)
;; https://www.reddit.com/r/dailyprogrammer/comments/3lf3i2/20150918_challenge_232_hard_redistricting_voting/

(defstruct region
  left
  right
  top
  bottom
  buffer)

(defun population (r)
  (loop with array = (region-buffer r)
     for x from (region-left r) below (region-right r) sum
       (loop for y from (region-top r) below (region-bottom r)
            sum (aref array y x))))

(defun read-problem (pathname)
  (with-input-from-file (s pathname)
    (with-input-from-string (str (read-line s))
      (let* ((line-count   (read str))
             (region-count (1+ line-count))
             (rows (read str))
             (cols (read str))
             (data (loop repeat rows for line = (read-line s) collect
                        (with-input-from-string (str line)
                          (loop repeat cols collect (read str)))))
             (buff (make-array (list rows cols) :initial-contents data)))
        (values buff region-count)))))

(defun make-basic-region (buff)
  (destructuring-bind (rows cols) (array-dimensions buff)
    (make-region :left 0 :top 0 :right cols :bottom rows :buffer buff)))

(defun map-splits (fn r)
  (let ((left (region-left r))
        (right (region-right r))
        (top (region-top r))
        (bottom (region-bottom r))
        (buff (region-buffer r)))
    (labels ((call-out (a b length)
               ;;(when (< (population b) (population a)) (psetf a b b a))
               (funcall fn a b length)))
      ;; vertical splits
      (loop for split from (1+ left) below right 
         with split-length = (- bottom top)
         for a = (make-region :left left  :right split :top top :bottom bottom :buffer buff)
         for b = (make-region :left split :right right :top top :bottom bottom :buffer buff)
         do (call-out a b split-length))
      ;; horizontal splits
      (loop for split from (1+ top) below bottom
         with split-length = (- right left)
         for a = (make-region :left left :right right :top top   :bottom split  :buffer buff)
         for b = (make-region :left left :right right :top split :bottom bottom :buffer buff)
         do (call-out a b split-length)))))

(defun population-ratio (a b)
  (/ (population a) (+ (population a)  (population b))))

(defun split-to-ratio (region a b)
  (let ((best most-positive-fixnum)
        (best-len most-positive-fixnum)
        (result nil)
        (goal-ratio (/ b (+ a b))))
    (map-splits
     (lambda (ra rb len)
       (let* ((pop-ratio (population-ratio rb ra))
              (diff (abs (- goal-ratio pop-ratio))))
         (flet ((save () (setf result (list ra rb)
                               best-len len
                               best   diff)))
           (cond
             ((or (null result) (< diff best)) 
              (save))
             ((and (= diff best) (< len best-len))
              (save))))))
     region)
    result))

(defun split-regions (r n)
  (if (= 1 n)
      (list r)
      (let* ((a  (floor n 2))
             (b  (ceiling n 2))
             (split (sort (split-to-ratio r a b) '< :key 'population)))
        (append (split-regions (first  split) a)
                (split-regions (second split) b)))))

(defun num->char (n)
  (elt "0123456789" n))

(defun make-output-board (buffer)
  (destructuring-bind (rows cols) (array-dimensions buffer)
    (let ((board (make-array (1+ (* rows 2)) :initial-element nil))
          (make-str (lambda (_)
                      (declare (ignore _))
                      (make-array (1+ (* cols 2)) 
                                  :element-type 'character
                                  :initial-element #\Space))))
      (prog1 board
        (map-into board make-str board)
        (loop for row below rows do
             (loop for col below cols
                  for x = (1+ (* col 2))
                  for y = (1+ (* row 2))
                  for num = (num->char (aref buffer row col)) do
                  (setf (aref (aref board y) x) num)))))))

(defun print-board (board &optional (stream *standard-output*))
  (loop for line across board do (format stream "~A~%" line)))

(defun draw-region (r board)
  (let ((left   (region-left r))
        (right  (region-right r))
        (top    (region-top r))
        (bottom (region-bottom r)))
    (labels
        ((set-char (x y ch)
           (let ((existing #1=(aref (aref board y) x)))
             (unless (char= existing ch)
               (if (char= existing #\Space)
                   (setf #1# ch)
                   (setf #1# #\+)))))
         (draw-vline (x)
           (loop for row from (* 2 top) to (* 2 bottom)
              do (set-char (* 2 x) row #\|)))
         (draw-hline (y)
           (loop for col from (* 2 left) to (* 2 right)
              do (set-char col (* 2 y) #\-))))
      (draw-vline left)
      (draw-vline right)
      (draw-hline top)
      (draw-hline bottom))))

(defun /u/mn-haskell-guy-error (pops avg)
  (reduce #'+ (mapcar (lambda (pop) (abs (- pop avg))) pops)))

(defun solve-file (pathname)
  (multiple-value-bind (buffer region-count) (read-problem pathname)
    (let* ((base (make-basic-region buffer))
           (solution (split-regions base region-count))
           (populations (mapcar 'population solution))
           (mean  (+ 0.0 (mean populations)))
           (board (make-output-board buffer)))
      (dolist (r solution) (draw-region r board))
      ;; (print-board board)
      (format t "Populations: ~A ~%" populations)
      (format t "Average:~A~%" mean)
      (format t "Error:~A~%" (/u/mn-haskell-guy-error populations mean))
      (format t "Std Dev:~A~%" (standard-deviation populations)))))
