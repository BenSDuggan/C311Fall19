#lang racket

(require racket/trace)

#|an interpreter decides the value of an expression|#
(define valof
  (λ (e env)
    (match e
      [`,n
       #:when (natural? n)
       n]
      [`,b
       #:when (boolean? b)
       b]
      [`(plas ,ne ,me)
       (+ (valof ne env) (valof me env))]
      [`(tymes ,ne ,me)
       (* (valof ne env) (valof me env))]
      [`(ef ,be ,then ,else)
       (if (valof be env) (valof then env) (valof else env))]
      #|the important three lines|#
      [`,y
       #:when (symbol? y)
       (apply-env env y)]
      [`(λ (,x) ,body)
       #:when (symbol? x)
       (λ (arg) (valof body (ext-env env x arg)))]
      [`(,rator ,rand)
       #|the result of (valof rator env) comes from the λ line|#
       #|the result of (valof rand env) is the arg for the λ line|#
       ((valof rator env) (valof rand env))])))

(define ext-env
  (λ (env x arg)
    `((,x . ,arg) . ,env)
    #;
    (λ (y) (if (eqv? y x) arg (apply-env env y)))))

(define apply-env
  (λ (env y)
    (match env
      ['() (error "oops")]
      [`((,x . ,arg) . ,env)
       (if (eqv? y x) arg (apply-env env y))])))

(define init-env
  (λ ()
    '()
    #;
    (λ (y)
      (error "oops"))))

(trace valof)

(define e₁
  '(ef #f (plas 3 5) (tymes 10 11)))

(define three
  (λ (step)
    (λ (base)
      (step (step (step base))))))
(define symbol3
  ((three add1) 0))

(define e₂
  '(((λ (a)
        (λ (b)
          (tymes a b)))
     3)
    5))



(valof e₂ (init-env))
