
(defpackage :branchie-utils
  (:use :cl)
  (:export join join-nl
           split-string))

(in-package :branchie-utils)


(defun split-string (str &optional (c #\Space))
  (if (position c str)
    (cons (subseq str 0 (position c str)) (split-string (subseq str (+ 1 (position c str)))))
    (cons str nil)))

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
