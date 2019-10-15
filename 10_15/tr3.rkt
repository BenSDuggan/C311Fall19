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

(define valof-cps
  (λ (exp env k)
    (λ ()
      (match exp
        [`,y
         #:when (symbol? y)
         (k (env y))]
        [`,n
         #:when (number? n)
         (k n)]
        [`(+ ,e₁ ,e₂)
         (valof-cps e₁ env
                    (λ (v₁)
                      (valof-cps e₂ env
                                 (λ (v₂)
                                   (k (+ v₁ v₂))))))]
        [`(λ (,x) ,b)
         (k (λ (a k)
              (valof-cps b (λ (y) (if (eqv? y x) a (env y))) k)))]
        [`(,rator ,rand)
         (valof-cps rator env
                    (λ (clos)
                      (valof-cps rand env
                                 (λ (a)
                                   (clos a k)))))]))))

(define eight
  (λ (jump-out)
    (fib-cps 5 jump-out)))

(define seven
  (λ (jump-out)
    (valof-cps '(((λ (a) (λ (b) (+ a b))) 5) 2)
                    (λ (y) (error "oops"))
                    jump-out)))

(define tramp₂
  (λ (th₁ th₂)
    (tramp₂ (th₁) (th₂))))

(let/cc jump-out
  (tramp₂ (seven jump-out) (eight jump-out)))

