
(defpackage :branchie-utils
  (:use :cl)
  (:export join join-nl
           split-string))

(in-package :branchie-utils)

(defun remove-nth (n l)
  (declare (type fixnum n) (type list l))
  "Remove N-th element from list L by copying
   everything except the N-th element."
  (nconc (subseq l 0 n) (nthcdr (1+ n) l)))

(defun join (sep l)
  (declare (type list l))
  "Joins a list of strings together with a separator recursively"
  (when (characterp sep)
    (setq sep (format nil "~a" sep)))

  (when (not sep)
    (setq sep ",")) ;set default value for separator if not defined
  
  (when (not (cdr l))
    (setq sep "")) ;remove separator if this is the end of the list
  (if l
    (concatenate 'string (car l) sep (join sep (cdr l)))
    ""))

(defmacro join-nl (&rest l)
  `(join #\Newline (list ,@l)))
