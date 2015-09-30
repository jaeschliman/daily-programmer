(ql:quickload '(:alexandria :cl-json))
(defpackage :challenge-20150831 (:use :cl :alexandria))
(in-package :challenge-20150831)
;; https://www.reddit.com/r/dailyprogrammer/comments/3j3pvm/20150831_challenge_230_easy_json_treasure_hunt/

(defun read-problem (pathname)
  (with-input-from-file (stream pathname)
    (json:with-decoder-simple-list-semantics
      (let ((json:*json-array-type* 'vector)
            (json:*json-identifier-name-to-lisp* 'identity))
        (json:decode-json-from-source stream)))))

(defun json-find-string (json string)
  (labels
      ((find-string (node path)
         (typecase node
           (string (when (string= node string)
                     (return-from json-find-string (reverse path))))
           (cons (loop for (key . value) in node do
                      (find-string value (cons key path))))
           (vector (loop for item across node for i upfrom 0 do
                        (find-string item (cons i path)))))))
    (find-string json nil)))

(defun solve-file (pathname)
  (let* ((json (read-problem pathname))
         (path (json-find-string json "dailyprogrammer")))
    (format t "~{~A~^ -> ~}" path)))

(ql:quickload :json-streams)

(defun json-stream-find-string (jstream string)
  (labels ((search-array (path)
             (loop for i upfrom 0
                for token = (search-value (cons i path))
                until (eq token :end-array)))
           (search-object (path)
             (loop for key = (json-streams:json-read jstream)
                until (eq key :end-object) do
                  (search-value (cons key path))))
           (search-value (path)
             (let ((token (json-streams:json-read jstream)))
               (when (and (stringp token) (string= token string))
                 (return-from json-stream-find-string (reverse path)))
               (case token
                 (:begin-array  (search-array path))
                 (:begin-object (search-object path))
                 (otherwise     token)))))
    (search-value nil)))

(defun solve-file2 (pathname)
  (with-input-from-file (s pathname)
    (let ((jstream (json-streams:make-json-input-stream s)))
      (json-streams:with-open-json-stream (js jstream)
        (let ((path (json-stream-find-string jstream "dailyprogrammer")))
          (format t "~{~A~^ -> ~}" path))))))
