;; Strategy pattern? Seriously? Here's how to do it in Common Lisp:
;;
;; We want to use a strategy to calculate the fibonacci series.
;; We don't need objects for this since functions in Lisp are first-class
;; citizens.
;;
;; But first things first: Three ways to compute fibonacci numbers

;; The classical recursive definition (very slow)
(defun fib-rec (n)
  (if (< n 2)
      n
      (+ (fib-rec (1- n)) (fib-rec (- n 2)))))

;; A much faster linear-time iterative version
(defun fib-iter (n)
  (let ((a 0)
	(b 1))
    (dotimes (i n a)
      (psetq a b
	     b (+ a b)))))

;; Last but not least: An improved recursive version using memoization
(let ((memo-table (make-hash-table)))
  (setf (gethash 0 memo-table) 0)
  (setf (gethash 1 memo-table) 1)
  (defun fib-memo (n)
    (or (gethash n memo-table)
	(setf
	 (gethash n memo-table)
	 (+ (fib-memo (1- n)) (fib-memo (- n 2)))))))

;; Create a list with algorithm names and their corresponding function
(defparameter *algorithm-list*
  (list (cons "iter" #'fib-iter)
	(cons "rec" #'fib-rec)
	(cons "memo" #'fib-memo)))

;; Create a hash table mapping algorithm names to functions using
;; a list of cons cells as its input, e. g.
;; '(("rec" . #'fib-rec) ("iter" . #'fib-iter))
(defun build-algorithm-table (algorithms)
  (let ((table (make-hash-table :test #'equalp)))
    (mapc
     (lambda (p) (setf (gethash (car p) table) (cdr p)))
     algorithms)
    table))

;; Last but not least: A function to try out different strategies/algorithms
;; Sample call: (calculate-fib "iter" 20)
(defun calculate-fib (algo-name x)
  (let* ((algos (build-algorithm-table *algorithm-list*))
	 (fn (gethash algo-name algos)))
    (if fn
	(funcall fn x)
	(format *error-output* "error: ~a not found!~%" algo-name))))
