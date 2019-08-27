#lang racket

#|car takes a list, returns the first thing|#
#;(car '(a b c))
#|cdr takes a list, returns everything but the first|#
#;(cdr (cdr '(a b c)))
#|cons takes two things, builds a pair|#
#|if the second thing is a list, then the resulted pair is a list|#
#;(cons 'cat 'dog)
#;(cons 'cat (cons 'dog '()))

(define factorial
  (λ (n)
    (cond
      #|base case|#
      [(zero? n) 1]
      #|natural recursion|#
      [else (* n (factorial (sub1 n)))])))

(define plus
  (λ (n m)
    (cond
      [(zero? n) m]
      [else (add1 (plus (sub1 n) m))])))

(define mult
  (λ (n m)
    (cond
      [(zero? n) 0]
      [else (plus m (mult (sub1 n) m))])))

#|check if something is in a list|#
(define member?
  (λ (a ls)
    (cond
      [(null? ls) #f]
      [else (or (eqv? (car ls) a)
                (member? a (cdr ls)))])))

(define length
  (λ (ls)
    (cond
      [(null? ls) 0]
      [else (add1 (length (cdr ls)))])))

(define length*
  (λ (ls)
    (cond
      [(null? ls) 0]
      [(list? (car ls)) (plus (length* (car ls))
                              (length* (cdr ls)))]
      [else (add1 (length* (cdr ls)))])))

(define flatten
  (λ (ls)
    (cond
      [(null? ls) '()]
      [(list? (car ls)) (append (flatten (car ls))
                                (flatten (cdr ls)))]
      [else (cons (car ls) (flatten (cdr ls)))])))

(define a-list-that-is-longer-and-nested
  '(((1) 2 (3 (()) (cat (42 dog))))))
