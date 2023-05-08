(defpackage :plang/transducer
  (:use :cl)
  (:nicknames :trans)
  (:shadow :append :mapcar)
  (:import-from :alexandria
                :compose)
  (:export :append
           :mapcar
           :mappend
           :filter
           :keep
           :id
           :transduce))

(in-package :plang/transducer)

(defun append ()
  (lambda (reducer)
    (lambda (result input)
      (reduce reducer input :initial-value result))))

(defun mapcar (fun)
  (lambda (reducer)
    (lambda (result input)
      (funcall reducer result (funcall fun input)))))

(defun mappend (fun)
  (compose (mapcar fun) (append)))

(defun filter (fun)
  (lambda (reducer)
    (lambda (result input)
      (if (funcall fun input)
          (funcall reducer result input)
          result))))

(defun keep (fun)
  (compose (mapcar fun) (filter #'identity)))

(defun id ()
  (lambda (reducer)
    (lambda (result input)
      (funcall reducer result input))))

(defun transduce (reducer transducer sequence &key initial-value)
  (reduce (funcall transducer reducer)
          sequence
          :initial-value initial-value))
