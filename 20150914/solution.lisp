(defpackage :challenge-20150914 (:use :cl :alexandria))
(in-package :challenge-20150914)
;; https://www.reddit.com/r/dailyprogrammer/comments/3kx6oh/20150914_challenge_232_easy_palindromes/

;; basic challenge
(defun just-letters (string)
  (remove-if-not 'alpha-char-p string))

(defun palindrome? (string &key (start-index 0) (end-index (length string)))
  (if (and (= start-index 0) (= end-index (length string)))
      (string= string (reverse string))
      (let* ((s (subseq string start-index end-index)))
        (string= s (reverse s)))))

(defun basic (string)
  (palindrome? (string-downcase (just-letters string))))

;; bonus challenge
#|
 build a reverse dictionary (as a trie)
 then, for each word,
 a) get all (reverse) prefixes of that word.
    for each prefix, if the remainder of the word is a palindrome,
    then count that pair.
 b) get all (reverse) suffixes of that word.
    for each suffix, if the suffix is a palindrome,
    then count that pair.
|#

(defun make-empty-trie () (list (list)))

(defun add-to-trie (trie string &optional (idx 0) (end-idx (length string)))
  (if (< idx end-idx)
      (let ((char (elt string idx)))
        (if-let (existing (assoc char (cdr trie)))
          (add-to-trie existing string (1+ idx))
          (let ((new (list char)))
            (push new (cdr trie))
            (add-to-trie new string (1+ idx)))))
      (pushnew '(:end . t) (cdr trie))))

(defun trie-map-matches (trie string fn &optional (idx 0) (end-idx (length string)))
  "calls fn for each index into string marking the ends of words found
   in trie"
  (labels ((check (trie pos)
             (when (<= pos end-idx)
               (when (assoc :end (cdr trie))
                 (funcall fn pos))
               (when (< pos end-idx)
                 (let ((char (elt string pos)))
                   (when-let (next (assoc char (cdr trie)))
                     (check next (1+ pos))))))))
    (check trie idx)))

(defun trie-map-suffixes (trie string fn &optional (idx 0) (end-idx (length string)))
  "calls fn on each string which is a suffix of string in trie"
  (labels ((to-end (trie idx)
             (if (< idx end-idx)
                 (let ((ch (elt string idx)))
                   (when-let (next (assoc ch (cdr trie)))
                     (to-end next (1+ idx))))
                 trie))
           (map-ends (trie acc)
             (dolist (pair (cdr trie))
               (if (eq :end (car pair))
                   (when acc (funcall fn (nreverse (coerce acc 'string))))
                   (let ((next (cons (car pair) acc)))
                     (declare (dynamic-extent next))
                     (map-ends pair next))))))
    (when-let (start (to-end trie idx))
      (map-ends start nil))))


(defun mapwords (fn)
  (with-open-file (s "enable1.txt" :direction :input)
    (loop for line = (read-line s nil) while line
         do (funcall fn (string-downcase (just-letters line))))))

;; build the reverse lookup trie
(defvar *reverse-dictionary* (make-empty-trie))
(mapwords (lambda (w) (add-to-trie *reverse-dictionary* (reverse w))))

(defun count-palindrome-pairs (word)
  "count each pair of palindromic words in the dictionary matching word"
  (let ((count 0)
        (is-pal (palindrome? word)))
    (labels ((suffix? (string)
               (when (palindrome? string)
                 (incf count)))
             ;; prefixes are given as end indexes into the search string
             (prefix? (idx)
               ;; check whether the remainder is a palindrome
               (when (palindrome? word :start-index idx)
                 ;; skip duplicates (e.g. aha aha)
                 (unless (and is-pal (= idx (length word)))
                   (incf count)))))
             (trie-map-suffixes *reverse-dictionary* word #'suffix?)
             (trie-map-matches  *reverse-dictionary* word #'prefix?))
    count))

;; expected output: 6501
(defun count-palindromes ()
  "count all palindromic pairs in the dictionary"
  (let ((count 0))
    (mapwords (lambda (word)
               (incf count (count-palindrome-pairs word))))
    count))
