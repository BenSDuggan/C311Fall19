#lang racket
(require "mk.rkt")

#|==|#
#|a goal|#
#;
(run 2 (x y)
  #|conde|#
  (condᵉ
   [(≡ 'cat x)
    (≡ 'dog y)
    (≡ x y)]
   [(≡ 'cat x)
    (≡ y x)]
   [(≡ x y)]))

(define (append xs ys)
  (cond
    [(eqv? '() xs) ys]
    [else (match-let ([`(,a . ,d) xs])
            (let ([res (append d ys)])
              `(,a . ,res)))]))

(defrel (appendᵒ xs ys o)
  (condᵉ
   [(≡ '() xs) (≡ ys o)]
   [(fresh (a d)
      (≡ `(,a . ,d) xs)
      (fresh (res)
        (≡ `(,a . ,res) o)
        (appendᵒ d ys res)))]))

#;
(run 1 q
  (symbolo 'cat))
(run 1 q
  (numbero 5))

(run* (xs ys)
      (absento 3 xs)
      (appendᵒ xs ys '(1 2 3 cat dog)))
