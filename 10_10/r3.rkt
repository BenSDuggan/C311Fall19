#lang racket
(require racket/trace)

#|set!|#
#|remove the formal parameters of apply-k and fib-cps|#
#|use global registers instread|#

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

#|three registers|#
(define fib-cps-n #f)
(define apply-k-v #f)
(define cc #f)

#|apply-k is serious|#
(define apply-k
  (λ () ;cc apply-k-v 
    (match cc
      [`(fib-sub2 ,fib-sub1 ,k)
       (begin [set! cc k]
              [set! apply-k-v (+ fib-sub1 apply-k-v)]
              (apply-k))]
      [`(fib-sub1 ,n ,k)
       (begin [set! cc (make-sub2-k apply-k-v k)]
              [set! fib-cps-n (sub2 n)]
              (fib-cps))]
      [`(init-k) apply-k-v])))

#|fib-cps is serious|#
(define fib-cps
  (λ () ; fib-cps-n cc
    (cond
      [(≤ fib-cps-n 1)
       (begin [set! cc cc]
              [set! apply-k-v 1]
              (apply-k))]
      [else
       (begin [set! cc (make-sub1-k fib-cps-n cc)]
              [set! fib-cps-n (sub1 fib-cps-n)]
              (fib-cps))])))

(begin [set! cc (make-init-k)]
       [set! fib-cps-n 5]
       (fib-cps))
