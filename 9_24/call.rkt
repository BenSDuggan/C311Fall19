#lang racket

(define valof
  (λ (exp env)
    (match exp
      [`,n
       #:when (number? n)
       n]
      [`,y
       #:when (symbol? y)
       (env y)]
      [`(λ (,x) ,body)
       (λ (arg)
         (valof body (λ (y) (if (eqv? y x) arg (env y)))))]
      [`(,rator ,rand)
       ((valof rator env) (valof rand env))])))

(define valof-cbr
  (λ (exp env)
    (match exp
      [`,n
       #:when (number? n)
       n]
      [`,y
       #:when (symbol? y)
       (unbox (env y))]
      [`(set! ,x ,e)
       (set-box! (env x) (valof-cbr e env))]
      [`(begin ,e₁ ,e₂)
       (begin (valof-cbr e₁ env)
              (valof-cbr e₂ env))]
      [`(λ (,x) ,body)
       (λ (b)
         (valof-cbr body (λ (y) (if (eqv? y x) b (env y)))))]
      #|the following line makes it call-by reference|#
      [`(,rator ,x)
       #:when (symbol? x)
       ((valof-cbr rator env) (env x))]
      [`(,rator ,rand)
       ((valof-cbr rator env) (box (valof-cbr rand env)))])))

#;
(valof-cbr '((λ (x)
               ((λ (y)
                  (begin (set! y 5)
                         x))
                x))
             10)
           (λ (y) (error "oops")))

(define Ω
  '((λ (x) (x x))
   (λ (x) (x x))))

(define valof-cbname
  (λ (exp env)
    (match exp
      [`,n
       #:when (number? n)
       n]
      [`,y
       #:when (symbol? y)
       ((env y))]
      [`(+ ,e₁ ,e₂)
       (begin
         (displayln "evaluating plus")
         (+ (valof-cbname e₁ env) (valof-cbname e₂ env)))]
      [`(λ (,x) ,body)
       (λ (arg)
         (valof-cbname body (λ (y) (if (eqv? y x) arg (env y)))))]
      [`(,rator ,rand)
       ((valof-cbname rator env) (λ () (valof-cbname rand env)))])))

(define valof-cbneed
  (λ (exp env)
    (match exp
      [`,n
       #:when (number? n)
       n]
      [`,y
       #:when (symbol? y)
       (let ([b (env y)])
         (let ([th (unbox b)])
           (let ([v (th)])
             (begin (set-box! b (λ () v))
                    v))))]
      [`(+ ,e₁ ,e₂)
       (begin
         (displayln "evaluating plus")
         (+ (valof-cbneed e₁ env) (valof-cbneed e₂ env)))]
      [`(λ (,x) ,body)
       (λ (arg)
         (valof-cbneed body (λ (y) (if (eqv? y x) arg (env y)))))]
      [`(,rator ,x)
       #:when (symbol? x)
       ((valof-cbneed rator env) (env x))]
      [`(,rator ,rand)
       ((valof-cbneed rator env) (box (λ () (valof-cbneed rand env))))])))

(valof-cbname `((λ (x) (+ x x))
                (+ 1 42))
              (λ (y) (error "oops")))
(valof-cbneed `((λ (x) (+ x x))
                (+ 1 42))
              (λ (y) (error "oops")))

