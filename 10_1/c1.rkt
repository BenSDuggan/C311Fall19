#lang racket

(require racket/trace)

(define ≤ <=)
(define sub2
  (λ (n)
    (sub1 (sub1 n))))

(define fib
  (λ (n)
    (cond
      [(≤ n 1) 1]
      [else (+ (fib (sub1 n)) (fib (sub2 n)))])))

(define init-k
  (λ ()
    (λ (v)
      v)))

(define make-sub2
  (λ (k fib-sub1)
    (λ (v)
      (apply-k k (+ fib-sub1 v)))))

(define make-sub1
  (λ (k n)
    (λ (v)
      (fib-cps (sub2 n) (make-sub2 k v)))))

(define apply-k
  (λ (k v)
    (k v)))

(define fib-cps
  (λ (n k)
    (cond
      [(≤ n 1) (apply-k k 1)]
      [else (fib-cps (sub1 n) (make-sub1 k n))])))

(fib-cps 10 (init-k))



