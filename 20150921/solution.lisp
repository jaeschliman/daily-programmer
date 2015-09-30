(defpackage :challenge-20150921 (:use :cl :alexandria))
(in-package :challenge-20150921)
;; https://www.reddit.com/r/dailyprogrammer/comments/3ltee2/20150921_challenge_233_easy_the_house_that_ascii/

(defun get-elt-or (seq n other)
  (if (< n (length seq))
        (elt seq n)
        other))

(defstruct box width height left)

(defparameter *box-width* 5)
(defparameter *box-height* 3)

(defun box-output-width (box)
  (1+ (* (box-width box) (1- *box-width*))))

(defun box-output-height (box)
  (+ (ceiling (- (box-output-width box) 2) 2)
     (* (box-height box) *box-height*)))

(defun box-right (box)
   (+ (box-left box) -1 (box-output-width box)))

(defun make-boxes (list)
  (let* ((lines  (coerce list 'vector))
         ;; account for varying line widths in the input
         (maxlen (reduce 'max lines :key 'length)))
    (labels ((getting (n)
               (lambda (seq) (get-elt-or seq n #\Space)))
             (height (n)
               (let ((strip (map 'vector (getting n) lines)))
                 (- (length strip)
                    (position #\* strip)))))
      (let ((result (list (make-box :width 1 :height (height 0)))))
        (loop for i from 1 below maxlen 
           for height = (height i)
           for last-box = (car result) do
             (if (= height (box-height last-box))
                 (incf (box-width last-box))
                 (push (make-box :height height :width 1) result)))
        (setf result (reverse (coerce result 'vector)))
        (loop with left = 0 for box across result do
             (setf (box-left box) left)
             (incf left (1- (box-output-width box))))
        result))))

(defun make-output-board (boxes)
  (let* ((overlap (lambda (box) (1- (box-output-width box))))
         (width   (1+ (reduce '+ boxes :key overlap)))
         (height  (reduce 'max boxes :key 'box-output-height))
         (mkstr   (lambda (_)
                    (declare (ignore _))
                    (make-array width :element-type 'character
                                :initial-element #\Space)))
         (board (make-array height :initial-element nil)))
    (prog1 board
      (map-into board mkstr board))))

(defun print-board (board &optional (stream *standard-output*))
  (loop for line across board do
       (format stream "~A~%" line)))

(defun put-ch (board x y ch)
  "write ch into output with y coord flipped"
  (let* ((maxidx (1- (length board)))
         (h (- maxidx y)))
    (setf (aref (aref board h) x) ch)))

(defun roof-height (box)
  "y coord for the base of the roof of box"
   (* (box-height box) (1- *box-height*)))

(defun draw-steeple (board box)
  (let* ((x (box-left box))
         (y (roof-height box))
         (w (- (box-output-width box) 2))
         (n (floor w 2))
         (x2 (+ x w 1)))
    (loop repeat n do
         (incf x)
         (incf y)
         (decf x2)
         (put-ch board x  y #\/)
         (put-ch board x2 y #\\))
    (incf x)
    (incf y)
    (put-ch board x y #\A)))

(defun draw-vline (board x y1 y2)
  (when (> y1 y2) (psetf y1 y2 y2 y1))
  (put-ch board x y1 #\+)
  (loop for y from (1+ y1) below y2 do
       (put-ch board x y #\|))
  (put-ch board x y2 #\+))

(defun draw-hline (board x1 x2 y)
  (when (> x1 x2) (psetf x1 x2 x2 x1))
  (put-ch board x1 y #\+)
  (loop for x from (1+ x1) below x2 do
       (put-ch board x y #\-))
  (put-ch board x2 y #\+))

(defun board-width (board)
  (length (elt board 0)))

(defun draw-doors (board boxes)
  (let* ((box (elt boxes (floor (length boxes) 2)))
         (x (+ (box-left box)
               (1- (floor (box-output-width box) 2)))))
    (put-ch board x 1 #\|)
    (put-ch board (+ x 2) 1 #\|)))

(defun draw-buildings (boxes board)
  (let* ((last-height 0))
    (labels ((draw (box)
               (let ((height (roof-height box)))
                 ;; draw the left wall
                 (draw-vline board (box-left box) (roof-height box) last-height)
                 (draw-steeple board box)
                 ;; draw the roofline
                 (draw-hline board (box-left box) (box-right box) height)
                 (setf last-height height))))
      (map nil #'draw boxes)
      ;; draw the final right wall
      (draw-vline board (box-right (last-elt boxes)) last-height 0)
      ;; draw the the ground 
      (draw-hline board 0 (1- (board-width board)) 0)
      ;; and the doors
      (draw-doors board boxes))))

(defun read-problem (pathname)
  (with-input-from-file (s pathname)
    (let* ((line-count (read-from-string (read-line s)))
           (lines      (loop repeat line-count collect (read-line s)))
           (boxes      (make-boxes lines)))
      boxes)))

(defun solve-file (pathname)
  (let* ((boxes (read-problem pathname))
         (board (make-output-board boxes)))
    (draw-buildings boxes board)
    (print-board board)))
