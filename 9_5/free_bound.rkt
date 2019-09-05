#lang racket

(require racket/trace)

#;
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

#|
free? with an accumulator
cenv is the accumulator
|#
(define free?
  (λ (e var cenv)
    (match e
      [`,y
       #:when (symbol? y)
       (and (eqv? y var) (not (memv y cenv)))]
      [`(λ (,x) ,body)
       #:when (symbol? x)
       (free? body var (cons x cenv))]
      [`(,rator ,rand)
       (or (free? rator var cenv) (free? rand var cenv))])))
#|
bound? with an accumulator
cenv is the accumulator
|#
(define bound?
  (λ (e var cenv)
    (match e
      [`,y
       #:when (symbol? y)
       (and (eqv? y var) (memv y cenv) #t)]
      [`(λ (,x) ,body)
       #:when (symbol? x)
       (bound? body var (cons x cenv))]
      [`(,rator ,rand)
       (or (bound? rator var cenv) (bound? rand var cenv))])))
#;
(define bound?
  (λ (e var)
    (match e
      [`,y
       #:when (symbol? y)
       #f]
      [`(λ (,x) ,body)
       #:when (symbol? x)
       (or (and (eqv? x var) (free? body var))
           (bound? body var))]
      [`(,rator ,rand)
       (or (bound? rator var) (bound? rand var))])))

;'(TabNine is cool)

;; (bound? '(λ (x) x) 'x)
;; (bound? '(λ (x) (λ (y) x)) 'x)
;; (bound? '(λ (x) (λ (x) x)) 'x)
;; (bound? '(λ (x) (λ (x) x)) 'x '())
;; (free? '(λ (x) (λ (y) (λ (z) x))) 'x '())

#|
deBruijn indices (lexical addresses)
the most recent λ is associated with 0
free vars don't have indices

(λ (x) x)
=>
(λ 0)

(λ (c)
  (λ (b)
    (λ (a)
      (c (a b)))))
=> 
(λ 
  (λ
    (λ
      (2 (0 1)))))

(λ (a)
  (λ (b)
    (λ (a)
      (c (a b)))))
=> 
(λ 
  (λ
    (λ
      (c (0 1)))))
|#


