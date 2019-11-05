#lang racket

(define-syntax kons
  (syntax-rules ()
    [(kons a d) (cons (box (λ () a)) (box (λ () d)))]))

(define kar
  (λ (pr)
    (let ([b (car pr)])
      (let ([th (unbox b)])
        (let ([v (th)])
          (begin
            (set-box! b (λ () v))
            v))))))

(define kdr
  (λ (pr)
    (let ([b (cdr pr)])
      (let ([th (unbox b)])
        (let ([v (th)])
          (begin
            (set-box! b (λ () v))
            v))))))

(define nats
  (λ (i)
    (kons i (nats (add1 i)))))

(define nats-from-1
  (nats 1))

#|stream -> list|#
(define take
  (λ (n $)
    (cond
      [(zero? n) '()]
      [else (cons (kar $)
                  (take (sub1 n) (kdr $)))])))

(define 1-20000
  (take 20000 nats-from-1))

(define multiple?
  (λ (n ls)
    (cond
      [(null? ls) #f]
      [(zero? (remainder n (car ls))) #t]
      [else (multiple? n (cdr ls))])))

(define primes
  (λ (nats previous-primes)
    (cond
      [(multiple? (kar nats) previous-primes)
       (primes (kdr nats) previous-primes)]
      [else
       (kons (kar nats)
             (primes (kdr nats)
                     (cons (kar nats) previous-primes)))])))

(define p (primes (nats 2) '()))


