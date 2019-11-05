#lang racket

(define random-iota
  (λ (n)
    (cond
      [(zero? n) '()]
      [else (cons (random 20) (random-iota (sub1 n)))])))

(define partition
  (λ (ls x)
    (cond
      [(null? ls) '(() ())]
      [(< (car ls) x)
       (match (partition (cdr ls) x)
         [`(,smaller ,larger)
          `(,(cons (car ls) smaller) ,larger)])]
      [else
       (match (partition (cdr ls) x)
         [`(,smaller ,larger)
          `(,smaller ,(cons (car ls) larger))])])))

#;
(partition (random-iota 20) 10)

(define sum
  (λ (ls)
    (cond
      [(null? ls) 0]
      [else (+ (car ls) (sum (cdr ls)))])))

(define average
  (λ (ls)
    (/ (sum ls) (length ls))))

(define sum×len
  (λ (ls)
    (cond
      [(null? ls) `(0 . 0)]
      [else (match (sum×len (cdr ls))
              [`(,s . ,l)
               `(,(+ (car ls) s) . ,(add1 l))])])))

(define average-sps
  (λ (ls)
    (match (sum×len ls)
      [`(,s . ,l)
       (/ s l)])))

#;
(average-sps '(1 2 3 4 5))

(define fib
  (λ (n)
    (cond
      [(<= n 1) 1]
      [else (+ (fib (sub1 n)) (fib (sub1 (sub1 n))))])))

#|memoization/dp|#
(define fib-sps
  (λ (n store)
    (cond
      [(assv n store) `(,(cdr (assv n store)) . ,store)]
      [(<= n 1) `(1 . ,store)]
      [else (match-let* ([`(,r1 . ,store1) (fib-sps (sub1 n) store)]
                         [`(,r2 . ,store2) (fib-sps (sub1 (sub1 n)) store1)]
                         [r (+ r1 r2)])
              `(,r . ((,n . ,r) . ,store2)))]
      #;
      [else (match (fib-sps (sub1 n) store)
              [`(,r1 . ,store1)
               (match (fib-sps (sub1 (sub1 n)) store1)
                 [`(,r2 . ,store2)
                  (let ([r (+ r1 r2)])
                    `(,r . ((,n . ,r) . ,store2)))])])])))

#;
(car (fib-sps 100 '()))

(define valof
  (λ (exp env store)
    (match exp
      [`,y
       #:when (symbol? y)
       `(,(env y) . ,(cons exp store))]
      [`,n
       #:when (number? n)
       `(,n . ,(cons exp store))]
      [`(λ (,x) ,body)
       `(,(λ (a store) (valof body (λ (y) (if (eqv? y x) a (env y))) store))
         .
         ,(cons exp store))]
      [`(,rator ,rand)
       #|orders matter in X-passing-style|#
       #|updating store gives different result to updating store₂|#
       (match-let* ([`(,clos . ,store₁) (valof rator env (cons exp store))]
                    [`(,a . ,store₂) (valof rand env store₁)])
         (clos a store₂))])))

#;
(valof '(((λ (a) (λ (b) a)) 5) 42)
       (λ (y) (error "oops"))
       '())
