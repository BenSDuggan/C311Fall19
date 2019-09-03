#lang racket

(define one 1)
(define two 2)
(define three 3)
(define a-list
  `(,one (,two three four)))

#;
((λ (little something)
   `(Mary had a ,little ,something))
 'big 'elephant)

#;
(define length
  (λ (ls)
    (cond
      [(null? ls) 0]
      [else (add1 (length (cdr ls)))])))

(define length
  (λ (ls)
    (match ls
      ['() 0]
      [`(,a . ,d) (add1 (length d))])))

(define double
  (λ (ls)
    (match ls
      [`() `()]
      [`(,a . ,d) `(,a . (,a . ,(double d)))]
      #;
      [`(,a . ,d) (cons a (cons a (double d)))])))

#|
A λ-expression is one of the followings.
- y             where y is a symbol
- (λ (x) body)  where x is a symbol and body is a λ-exp
- (rator rand)  where rator and rand are λ-exps
|#

(define copy/lambda
  (λ (e)
    (match e
      [`,y
       #:when (symbol? y)
       y]
      [`(λ (,x) ,body)
       #:when (symbol? x)
       `(lambda (,x) ,(copy/lambda body))]
      [`(,rator ,rand)
       `(,(copy/lambda rator) ,(copy/lambda rand))])))

(define some-exp-with-no-free-vars
  '(λ (x)
     (λ (y)
       (x y))))

(define some-exp-with-a-free-x
  '(λ (y)
     (x y)))

(define some-exp-with-free-xs
  '((λ (y)
      (x y))
    (λ (b)
      (x b))))

(define some-exp-with-no-free-vars₂
  '(λ (x)
     ((λ (y)
        (x y))
      (λ (b)
        (x b)))))

(define count-λ
  (λ (e)
    (match e
      [`,y
       #:when (symbol? y)
       0]
      [`(λ (,x) ,body)
       #:when (symbol? x)
       (add1 (count-λ body))]
      [`(,rator ,rand)
       (+ (count-λ rator) (count-λ rand))])))

#|decides whether var occurs free in e|#
(define free?
  (λ (e var)
    (match e
      [`,y
       #:when (symbol? y)
       (eqv? y var)]
      [`(λ (,x) ,body)
       #:when (symbol? x)
       (and (not (eqv? x var)) (free? body var))]
      [`(,rator ,rand)
       (or (free? rator var) (free? rand var))])))
