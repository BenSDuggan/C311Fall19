#lang racket
(require "monads.rkt")
(require racket/trace)

#|
left identity:
(bind (inj a) f) ≡ (f a)
|#

#;
(bind (Just 5)
      (λ (a)
        (Just (+ a a))))
#;
((λ (a)
   (Just (+ a a)))
 5)


#|
right identity:
(bind ma inj) ≡ ma
|#
#;
(bind (Just 5) Just)

#|
associativity:
(bind (bind ma f) g) ≡ (bind ma (λ (a) (bind (f a) g)))
|#
#;
(bind (bind (Just 5)
            (λ (a)
              (Just (+ a a))))
      (λ (a)
        (Just (* a a))))
#;
(bind (Just 5)
      (λ (a)
        (bind ((λ (a)
                 (Just (+ a a))) a)
              (λ (a)
                (Just (* a a))))))

#|
lists are monads:
inj-list : (λ (a) (cons a '()))
bind-list : append-map
fail-list : '()
|#
(define filter
  (λ (ls p)
    (bind-list ls
               (λ (a)
                 (if (p a)
                     (inj-list a)
                     (fail-list))))))

#;
(filter '(1 2 3 4 5 6) even?)

(define product
  (λ (xs ys)
    (bind-list xs
               (λ (x)
                 (bind-list ys
                            (λ (y)
                              (inj-list (cons x y))))))))

#;
(product '(1 2 3) '(cat dog))

(define fact-cps
  (λ (n k)
    (cond
      [(zero? n) (k 1)]
      [else (fact-cps (sub1 n)
                      (λ (r)
                        (k (* n r))))])))
#;
(fact-cps 5 (λ (v) v))

(define fact
  (λ (n)
    (cond
      [(zero? n) (inj-cont 1)]
      [else (bind-cont (fact (sub1 n))
                       (λ (r)
                         (inj-cont (* n r))))])))
(trace fact)

#;
((run-cont
  (fact 5))
 (λ (v) (+ v v)))

(define valof
  (λ (exp env)
    (match exp
      [`,y
       #:when (symbol? y)
       (inj-cont (env y))]
      [`,n
       #:when (number? n)
       (inj-cont n)]
      [`(zero? ,e)
       (go-on ([r (valof e env)])
         (inj-cont (zero? r)))]
      [`(+ ,e₁ ,e₂)
       (go-on ([r₁ (valof e₁ env)]
               [r₂ (valof e₂ env)])
         (inj-cont (+ r₁ r₂)))]
      [`(let/cc ,k-name ,body)
       (callcc (λ (k) (valof body (λ (y) (if (eqv? y k-name) k (env y))))))]
      [`(λ (,x) ,body)
       (inj-cont (λ (a) (valof body (λ (y) (if (eqv? y x) a (env y))))))]
      [`(,rator ,rand)
       (go-on ([clos (valof rator env)]
               [a (valof rand env)])
         (clos a))])))

#;
((run-cont
  (valof '(+ 10 (let/cc k (+ 5 (k (k 6))))) (λ (y) (error "oops"))))
 (λ (v) v))

#;
(run-writer
 (bind-writer (tell 1)
              (λ (_)
                (tell 2))))

(define divide
  (λ (ls)
    (cond
      [(null? ls) (inj-writer '())]
      [(zero? (car ls)) (go-on ([_ (tell 'indeterminate-value)])
                          (divide (cdr ls)))]
      [else  (go-on ([r (divide (cdr ls))])
               (inj-writer (cons (/ 10 (car ls)) r)))])))

(run-writer
 (divide '(1 2 0 0 3 0 0)))

