#lang racket

#|cpsed fib, RI|#

(define ≤ <=)
(define sub2 (compose sub1 sub1))

(define apply-k
  (λ (k v)
    (match k
      [`(fib-sub2 ,fib-sub1 ,k) (apply-k k (+ fib-sub1 v))]
      [`(fib-sub1 ,n ,k) (fib-cps (sub2 n) (make-sub2-k v k))]
      [`(init-k) v])))

(define make-sub2-k
  (λ (fib-sub1 k)
    `(fib-sub2 ,fib-sub1 ,k)
    #;
    (λ (v)
      (apply-k k (+ fib-sub1 v)))))

(define make-sub1-k
  (λ (n k)
    `(fib-sub1 ,n ,k)
    #;
    (λ (v)
      (fib-cps (sub2 n) (make-sub2-k v k)))))

(define make-init-k
  (λ ()
    `(init-k)
    #;
    (λ (v) v)))

(define fib-cps
  (λ (n k)
    (cond
      [(≤ n 1) (apply-k k 1)]
      [else (fib-cps (sub1 n) (make-sub1-k n k))])))


(fib-cps 5 (make-init-k))
