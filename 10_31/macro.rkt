#lang racket

(define-syntax letₙ
  (syntax-rules ()
    [(letₙ () body)
     body]
    [(letₙ ([x₀ e₀] [x e] ...) body)
     ((λ (x₀ x ...) body) e₀ e ...)]))

#;
(letₙ ([x 5] [y 10])
      (+ x y))

(define-syntax let*ₙ
  (syntax-rules ()
    [(let*ₙ () body) body]
    [(let*ₙ ([x₀ e₀] [x e] ...) body)
     (let ([x₀ e₀])
       (let*ₙ ([x e] ...)
              body))]))

#;
(let*ₙ ([x 5] [x (+ x x)] [x (+ x x)])
       x)

(define-syntax let!
  (syntax-rules (=)
    [(let! body) body]
    [(let! [x₀ = e₀] [x = e] ... body)
     (begin (set! x₀ e₀)
            (let! [x = e] ... body))]))

#|
(define x #f)
(define y #f)
(let! (x = 5)
      (y = 10)
      (+ x y))
|#

(define-syntax and
  (syntax-rules ()
    [(and) #t]
    [(and e) e]
    [(and e₀ e ...)
     (if e₀ (and e ...) #f)]))

#;
(and 1 2 3)

(define-syntax or
  (syntax-rules ()
    [(or) #f]
    [(or e₀ e ...)
     (let ([foo e₀])
       (if foo foo (or e ...)))]))

#;
(let ([foo 5])
  (or (begin (displayln 'hey) foo) 2 3))

(define-syntax cond
  (syntax-rules (=>)
    [(cond)
     (void)]
    [(cond [t₀ => f] more ...)
     (let ([v t₀])
       (if v (f v) (cond more ...)))]
    [(cond [t₀ r₀ r ...] more ...)
     (if t₀ (begin r₀ r ...) (cond more ...))]
    [(cond [t₀ r₀] [t r] ...)
     (if t₀ r₀ (cond [t r] ...))]))

(cond
  [(eqv? 'cat 'dog) 'oops]
  [(assv 'x '((y . 10) (x . 5)))
   =>
   (λ (pr) (cdr pr))]
  [#t 1 2 3 4]
  [#t 'hey])
