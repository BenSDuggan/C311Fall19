#lang racket

#|registerization|#
#|changed the global register k to continuation
  to avoid namin collisions
|#

(define ≤ <=)
(define sub2 (λ (n) (sub1 (sub1 n))))

#|constructors are simple|#
(define make-fib2-k
  (λ (fib-sub1 k)
    `(fib2-k ,fib-sub1 ,k)))

#|constructors are simple|#
(define make-fib1-k
  (λ (n k)
    `(fib1-k ,n ,k)))

#|constructors are simple|#
(define make-init-k
  (λ (jump-out)
    `(init-k ,jump-out)))

(define continuation #f)
(define apply-k-v #f)
(define fib-cps-n #f)
(define pc #f)

#|apply-k is a serious function and needs registerization|#
(define apply-k
  (λ ()
    (match continuation
      [`(fib2-k ,fib-sub1 ,k)
       (begin (set! continuation k)
              (set! apply-k-v (+ fib-sub1 apply-k-v))
              (set! pc apply-k))]
      [`(fib1-k ,n ,k)
       (begin (set! continuation (make-fib2-k apply-k-v k))
              (set! fib-cps-n (sub2 n))
              (set! pc fib-cps))]
      [`(init-k ,jump-out) (jump-out apply-k-v)])))

#|fib-cps is a serious function and needs registerization|#
(define fib-cps
  (λ ()
    (cond
      [(≤ fib-cps-n 1)
       (begin (set! continuation continuation)
              (set! apply-k-v 1)
              (set! pc apply-k))]
      [else
       (begin (set! continuation (make-fib1-k fib-cps-n continuation))
              (set! fib-cps-n (sub1 fib-cps-n))
              (set! pc fib-cps))])))

(define loop
  (λ ()
    (begin (pc)
           (loop))))

(let/cc jump-out
  (begin (set! continuation (make-init-k jump-out))
         (set! fib-cps-n 5)
         (set! pc fib-cps)))

(loop)


