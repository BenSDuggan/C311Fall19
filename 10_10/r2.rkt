#lang racket

(require racket/trace)

#|let*|#
#|make all fib-cps and apply-k calls look the same|#

#;
(let* ([x 5]
       [x (+ x 10)]
       [x (* 3 x)])
  x)

(define ≤ <=)
(define sub2 (compose sub1 sub1))

(define make-sub2-k
  (λ (fib-sub1 k)
    `(fib-sub2 ,fib-sub1 ,k)))

(define make-sub1-k
  (λ (n k)
    `(fib-sub1 ,n ,k)))

(define make-init-k
  (λ ()
    `(init-k)))

#|apply-k is serious|#
(define apply-k
  (λ (k v)
    (match k
      [`(fib-sub2 ,fib-sub1 ,k)
       (let* ([cc k]
              [apply-k-v (+ fib-sub1 v)])
         (apply-k cc apply-k-v))]
      [`(fib-sub1 ,n ,k)
       (let* ([cc (make-sub2-k v k)]
              [fib-cps-n (sub2 n)])
         (fib-cps fib-cps-n cc))]
      [`(init-k) v])))
(trace apply-k)

#|fib-cps is serious|#
(define fib-cps
  (λ (n k)
    (cond
      [(≤ n 1)
       (let* ([cc k]
              [apply-k-v 1])
         (apply-k cc apply-k-v))]
      [else
       (let* ([cc (make-sub1-k n k)]
              [fib-cps-n (sub1 n)])
         (fib-cps fib-cps-n cc))])))
(trace fib-cps)

(let* ([cc (make-init-k)]
       [fib-cps-n 5])
  (fib-cps fib-cps-n cc))
