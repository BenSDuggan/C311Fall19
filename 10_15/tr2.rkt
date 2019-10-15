#lang racket

(define ≤ <=)
(define sub2 (compose sub1 sub1))

(define fib-cps
  (λ (n k)
    (λ ()
      (cond
        [(≤ n 1) (k 1)]
        [else (fib-cps (sub1 n)
                       (λ (fsub1)
                         (fib-cps (sub2 n)
                                  (λ (fsub2)
                                    (k (+ fsub1 fsub2))))))]))))

(define tramp
  (λ (th)
    (tramp (th))))

(let/cc jump-out
  (tramp (fib-cps 5 jump-out)))

