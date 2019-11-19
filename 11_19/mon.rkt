#lang racket
(require "monads.rkt")

(struct Nothing
  ()
  #:transparent)
(struct Just
  (result)
  #:transparent)

#|Symb -> List (Symb × Nat) -> Maybe Nat|#
(define find
  (λ (x ls)
    (cond
      [(null? ls) (Nothing)]
      [(eqv? (car (car ls)) x) (Just (cdr (car ls)))]
      [else (find x (cdr ls))])))

#|Maybe A -> (A -> Maybe B) -> Maybe B|#
(define bind-maybe
  (λ (ma f)
    (match ma
      [(Just a) (f a)]
      [(Nothing) (Nothing)])))

#|Symb -> List (Symb × Nat) -> Maybe Nat|#
(define find-then-half
  (λ (x ls)
    (bind-maybe (find x ls)
                (λ (a)
                  (Just (/ a 2))))
    #;
    (match (find x ls)
      [(Just a) (Just (/ a 2))]
      [(Nothing) (Nothing)])))

#;
(find-then-half 'x `((x . 42) (y . 10) (z . 42)))

#;
(define sum 0)
#;
(define len×sum
  (λ (ls)
    (cond
      [(null? ls) 0]
      [else (begin (set! sum (+ (car ls) sum))
                   (add1 (len×sum (cdr ls))))])))

#|List Nat -> State Nat Nat|#
(define len×sum
  (λ (ls)
    (cond
      [(null? ls) (inj-state 0)]
      [else (go-on ([sum (get)]
                    [_ (put (+ (car ls) sum))]
                    [r (len×sum (cdr ls))])
              (inj-state (add1 r)))]
      #;
      [else (bind-state (get)
                        (λ (sum)
                          (bind-state (put (+ (car ls) sum))
                                      (λ (_)
                                        (bind-state (len×sum (cdr ls))
                                                    (λ (r)
                                                      (inj-state (add1 r))))))))])))

(define average
  (λ (ls)
    (go-on ([len (len×sum ls)]
            [sum (get)])
      (inj-state (/ sum len)))
    #;
    (bind-state (len×sum ls)
                (λ (len)
                  (bind-state (get)
                              (λ (sum)
                                (inj-state (/ sum len))))))))

(define half×sum
  (λ (n)
    (go-on ([sum (get)]
            [_ (put (+ n sum))])
      (cond
        [(odd? n) (inj-state n)]
        [(even? n) (half×sum (/ n 2))]))
    #;
    (cond
      [(odd? n)
       (go-on ([sum (get)]
               [_ (put (+ n sum))])
         (inj-state n))]
      [(even? n)
       (go-on ([sum (get)]
               [_ (put (+ n sum))])
         (half×sum (/ n 2)))])))

((run-state
  (half×sum 80))
 0)

#;
((run-state
  (average '(1 2 3 4 5 6)))
 0)

#;
((run-state
  (bind-state (inj-state 5)
              (λ (five)
                (bind-state (inj-state (+ five 10))
                            (λ (x)
                              (bind-state (put 'blah)
                                          (λ (_)
                                            (inj-state (zero? x)))))))))
 0)
