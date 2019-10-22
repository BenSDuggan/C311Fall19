#lang racket

#;
(foldr
 (λ (a sum) (begin (displayln a) (+ a sum)))
 0
 '(1 2 3 5))

#;
(call/cc
 (λ (k) (continuation? k)))

#;
(foldr
 (λ (f ls) (call/cc (λ (k) (f k))))
 '()
 (list procedure? number? list?))

(define bi-tramp
  (λ (th₁ th₂)
    (bi-tramp (th₁) (th₂))))

(define Ω
  (λ (k)
    ((λ (x k) (λ () (x x k)))
     (λ (x k) (λ () (x x k)))
     k)))

(define fact
  (λ (n k)
    (λ ()
      (if (zero? n)
          (k 1)
          (fact (sub1 n)
                (λ (v) (k (* n v))))))))

#;
(call/cc
 (λ (k)
   (bi-tramp (Ω k)
             (fact 5 k))))

#;
(define arith
  (λ (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) 0]
      [(zero? (remainder (car ls) 3))
       (arith)])))

(define multi
  (λ (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) 0]
      [else (multi (cdr ls)
                   (λ (v)
                     (k (* (car ls) v))))])))

#;
(multi '(1 2 3 0 4) (λ (v) v))

#;
(((λ (x)
    (λ (y)
      (* y y)))
  ((λ (x) (x x)) (λ (x) (x x))))
 120)

(define unf-cps
  (λ (v p? k)
    (k (λ (f g k)
         (p? v
             (λ (b)
               (cond
                 [b (k v)]
                 [else
                  (g v
                     (λ (gv)
                       (unf-cps gv p?
                                (λ (blah)
                                  (f v blah g k)))))])))))))
