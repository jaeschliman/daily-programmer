(ql:quickload :alexandria)
(defpackage :challenge-20150923 (:use :cl :alexandria))
(in-package :challenge-20150923)
;; https://www.reddit.com/r/dailyprogrammer/comments/3m2vvk/20150923_challenge_233_intermediate_game_of_text/

(defun file-lines (pathname)
  (coerce (with-input-from-file (s pathname)
            (loop for line = (read-line s nil nil)
               while line collect line))
          'vector))

(defun make-board (w h)
  (make-array (list h w) :element-type 'character :initial-element #\Space))

(defmacro do-xy ((board &optional after-line) &body body)
  (once-only (board)
    `(loop for y below (array-dimension ,board 0) do
          (loop for x below (array-dimension ,board 1) do
               (progn ,@body))
          (progn ,after-line))))

(defun print-board (board &optional (stream *standard-output*))
  (do-xy (board (write-char #\Newline stream))
    (write-char (aref board  y x) stream)))

(defun read-problem (pathname)
  (let* ((lines  (file-lines pathname))
         (height (length lines))
         (width  (reduce 'max lines :key 'length))
         (board  (make-board width height)))
    (prog1 board
      (loop for line across lines for y upfrom 0 do
           (loop for char across line for x upfrom 0 do
              (setf (aref board y x) (aref line x)))))))

(defun char-at (board x y)
  (if (and (< -1 x (array-dimension board 1))
           (< -1 y (array-dimension board 0)))
      (aref board y x)
      #\Space))

(defvar *neighbors*
  (loop for a in #1='(-1 0 1) append
       (loop for b in #1# unless (and (= 0 a) (= 0 b)) collect (cons a b))))

(defun step-board (board)
  (let* ((width  (array-dimension board 1))
         (height (array-dimension board 0))
         (output (make-board width height)))
    (prog1 output
      (let (lastchar sum thischar)
        (do-xy (board)
          (setf sum 0 lastchar nil thischar (aref board y x))
          (loop for (a . b) in *neighbors* 
             for char = (char-at board (+ x a) (+ y b)) do
               (unless (char= char #\Space)
                 (setf lastchar char)
                 (incf sum)))
          (if (char= thischar #\Space)
              (when (= sum 3)
                (setf (aref output y x) lastchar))
              (when (<= 2 sum 3)
                (setf (aref output y x) thischar))))))))

(defun solve-file (pathname)
  (let ((board (read-problem pathname)))
    (format t "~C[2J" #\Esc)
    (print-board board)
    (loop do
         (sleep 0.5)
         (format t "~C[2J" #\Esc)
         (setf board (step-board board))
         (print-board board))
    (values)))

(solve-file "solution.lisp")
