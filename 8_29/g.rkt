#lang racket

(define +
  (λ (n m)
    (cond
      [(zero? m) n]
      [else (add1 (+ n (sub1 m)))])))

(define *
  (λ (n m)
    (cond
      [(zero? m) 0]
      [else (+ n (* n (sub1 m)))])))

(define ↑
  (λ (n m)
    (cond
      [(zero? m) 1]
      [else (* n (↑ n (sub1 m)))])))

(define ⇑
  (λ (n m)
    (cond
      [(zero? m) 1]
      [else (↑ n (⇑ n (sub1 m)))])))

#;
(define G
  (λ (k)
    (cond
      [(zero? k)
       (λ (n m)
         (cond
           [(zero? m) n]
           [else (add1 ((G k) n (sub1 m)))]))]
      [(zero? (sub1 k))
       (λ (n m)
         (cond
           [(zero? m) 0]
           [else ((G (sub1 k)) n ((G k) n (sub1 m)))]))]
      [else
       (λ (n m)
         (cond
           [(zero? m) 1]
           [else ((G (sub1 k)) n ((G k) n (sub1 m)))]))])))

(define G
  (λ (k)
    (λ (n m)
      (cond
        [(zero? m)
         (cond
           [(zero? k) n]
           [(zero? (sub1 k)) 0]
           [else 1])]
        [(zero? k)
         (add1 ((G k) n (sub1 m)))]
        [else
         ((G (sub1 k)) n ((G k) n (sub1 m)))]))))
