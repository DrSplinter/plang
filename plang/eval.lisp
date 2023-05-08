(defpackage :plang/eval
  (:use :cl)
  (:nicknames :eval)
  (:shadow :funcall :find)
  (:import-from :plang/expr)
  (:import-from :plang/transducer)
  (:import-from :alexandria
                :compose
                :plist-alist)
  (:export :find
           :where
           :?input))

(in-package :plang/eval)


;;;
;;; PROJECT
;;;

(defun projectionp (expr)
  (and (listp expr)
       (symbolp (car expr))))

(defun match (row projections)
  (reduce (lambda (env projection)
            (destructuring-bind (key . pattern) projection
              (let ((new (expr:match (getf row key) pattern)))
                (expr:merge env new))))
          (plist-alist projections)
          :initial-value (expr:env)))

(defun projection (expr)
  (destructuring-bind (table &rest projections) expr
    (compose
     (trans:mappend
      (lambda (env)
        (mapcar (lambda (row)
                  (catch 'expr:failed
                    (expr:merge env (match row projections))))
                (getf (expr:subst '?input env) table))))
     (trans:filter #'identity))))


;;;
;;; CALL
;;;

(defun callp (expr)
  (and (listp expr)
       (listp (car expr))))

(defun call (expr)
  (destructuring-bind (call &rest rest) expr
    (declare (ignore rest))
    (trans:mapcar
     (lambda (env)
       (list (eval (expr:env-let env call))
             env)))))

(defun funcallp (expr)
  (and (callp expr)
       (= 2 (length expr))))

(defun funcall (expr)
  (destructuring-bind (call var) expr
    (declare (ignore call))
    (compose
     (call expr)
     (trans:mapcar
      (lambda (val)
        (destructuring-bind (val env) val
          (expr:merge env (expr:match var val))))))))

(defun predicatep (expr)
  (and (callp expr)
       (= 1 (length expr))))

(defun predicate (expr)
  (compose
   (call expr)
   (trans:keep
    (lambda (val)
      (destructuring-bind (val env) val
        (when val env))))))


;;;
;;; SUBQUERY
;;;

(defun find-subqueryp (expr)
  (and (funcallp expr)
       (eql :find (caar expr))))

(defun find-all-subqueryp (expr)
  (and (funcallp expr)
       (eql :find-all (caar expr))))

(declaim (function find-subquery find-all-subquery))

(defun to-pipeline (exprs)
  (if exprs
      (destructuring-bind (expr &rest rest) exprs
        (compose
         (cond ((find-subqueryp expr)
                (find-subquery expr))
               ((find-all-subqueryp expr)
                (find-all-subquery expr))
               ((projectionp expr)
                (projection expr))
               ((predicatep expr)
                (predicate expr))
               ((funcallp expr)
                (funcall expr))
               (t (error "Unknown where clause ~A." expr)))
         (to-pipeline rest)))
      (trans:id)))

(defun where (clause env)
  (trans:transduce (lambda (result env)
                     (cons env result))
                   (to-pipeline clause)
                   (list env)
                   :initial-value nil))

(defun find (clause env)
  (eval (expr:env-let env clause)))


;;FIXME: trochu zuslechtit
(defun find-subquery (expr)
  (destructuring-bind ((&key find where) var) expr
    (trans:mappend
     (lambda (env)
       (mapcar (lambda (subenv)
                 (expr:merge env (expr:match var (find find subenv))))
               (where where env))))))

(defun find-all-subquery (expr)
  (destructuring-bind ((&key find-all where) var) expr
    (trans:mapcar
     (lambda (env)
       (expr:merge env
                   (expr:match var
                     (mapcar (lambda (subenv) (find find-all subenv))
                             (where where env))))))))
