#lang racket
(require "parenthec.rkt")

(define-union continuation
  (fib-sub2 fib-sub1 k)
  (fib-sub1 n k)
  (init-k))

(define fib-cps-n #f)
(define apply-k-v #f)
(define cc #f)

(define apply-k
  (λ () 
    (union-case cc continuation
      [(fib-sub2 fib-sub1 k)
       (begin [set! cc k]
              [set! apply-k-v (+ fib-sub1 apply-k-v)]
              (apply-k))]
      [(fib-sub1 n k)
       (begin [set! cc (continuation_fib-sub2 apply-k-v k)]
              [set! fib-cps-n (sub1 (sub1 n))]
              (fib-cps))]
      [(init-k) apply-k-v])))

(define fib-cps
  (λ ()
    (cond
      [(<= fib-cps-n 1)
       (begin [set! cc cc]
              [set! apply-k-v 1]
              (apply-k))]
      [else
       (begin [set! cc (continuation_fib-sub1 fib-cps-n cc)]
              [set! fib-cps-n (sub1 fib-cps-n)]
              (fib-cps))])))

(begin [set! cc (continuation_init-k)]
       [set! fib-cps-n 5]
       (fib-cps))
