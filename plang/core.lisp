(defpackage :plang/core
  (:use :cl)
  (:nicknames :plang)
  (:import-from :plang/expr)
  (:import-from :plang/eval)
  (:export :interpret
           :query))

(in-package :plang/core)


(defun interpret (query db)
  (eval:find '?output
             (first (eval:where `((,query ?output))
                                (expr:match 'eval:?input db)))))

(defmacro query (db &body clauses)
  `(interpret ',clauses ,db))
