(defpackage :plang/expr
  (:use :cl)
  (:nicknames :expr)
  (:shadow :merge
           :var
           :subst)
  (:export :unify
           :==
           :match
           :failed
           :varp
           :env
           :add
           :merge
           :subst
           :env-let
           :vars))

(in-package :plang/expr)


;;;
;;; BIND
;;;

(defun bind (var val)
  (list var val))

(defun var (bind)
  (car bind))

(defun val (bind)
  (cadr bind))

(defun varp (symbol)
  (and (symbolp symbol)
       (char= #\? (aref (symbol-name symbol) 0))))

(defun var= (var1 var2)
  (string= (symbol-name var1) (symbol-name var2)))

(declaim (function match subst))

(defun update (old new)
  (bind (var old) (subst (val old) (match (val old) (val new)))))

(defun assign (expr bind)
  (cl:subst (val bind) (var bind) expr))


;;;
;;; ENV
;;;

(defun env (&rest binds)
  (cons (list t t) binds))

(defun add (new env) 
  (let* ((old (assoc (var new) env)))
    (cons (if old (update old new) new) env)))

(defun merge (env1 env2)
  (reduce #'add env1 :initial-value (rest env2) :from-end t))

(defun subst (expr env)
  (reduce #'assign env :initial-value expr))


;;;
;;; MATCH
;;;

(defun match (expr1 expr2)
  (cond ((and (consp expr1) (consp expr2))
         (merge (match (car expr1) (car expr2))
                (match (cdr expr1) (cdr expr2))))
        ((and (varp expr1) (varp expr2))
         ;; FIXME: cyklicka zavislost?
         (when (var= expr1 expr2) (env)))
        ((varp expr1)
         (env (bind expr1 expr2)))
        ((varp expr2)
         (env (bind expr2 expr1)))
        ((equalp expr1 expr2)
         (env))
        (t (throw 'failed nil))))

(defun == (expr1 expr2)
  (catch 'failed (match expr1 expr2)))

(defun unify (expr1 expr2)
  (let ((env (== expr1 expr2)))
    (values (when env (subst expr1 env))
            (not (not env)))))


;;;
;;; EVALUATION
;;;

(defun vars (expr)
  (labels ((extract (expr accum)
             (cond ((varp expr)
                    (cons expr accum))
                   ((consp expr)
                    (extract (cdr expr) (extract (car expr) accum)))
                   (t accum))))
    (remove-duplicates (extract expr nil))))

(defun env-let (env &rest body)
  `(let ,(mapcar (lambda (bind)
                   `(,(var bind) ',(val bind)))
                 (remove-duplicates (rest env)
                                    :from-end t
                                    :key #'car))
     (declare (ignorable ,@(mapcar #'first (rest env))))
     ,@body))
