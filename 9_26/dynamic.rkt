#lang racket

#|store-passing style|#
(define sum×length
  (λ (ls len)
    (match ls
      ['() `(0 ,len)]
      [`(,a . ,d)
       (match (sum×length d len)
         [`(,sum ,len)
          `(,(+ a sum) ,(add1 len))])])))

(define average
  (λ (ls)
    (match (sum×length ls 0)
      [`(,sum ,len)
       (/ sum len)])))

#;
(average '(1 2 3 4 5))


#|an interpreter of dynamic scope|#
(define valof
  (λ (exp env)
    (match exp
      [`,y
       #:when (symbol? y)
       `(,(env y) ,env)]
      [`,b
       #:when (boolean? b)
       `(,b ,env)]
      [`,n
       #:when (number? n)
       `(,n ,env)]
      [`(null? ,e)
       (match (valof e env)
         [`(,v ,env)
          `(,(null? v) ,env)])]
      [`(quote ,e)
       `(,e ,env)]
      [`(car ,e)
       (match (valof e env)
         [`(,ls ,env)
          `(,(car ls) ,env)])]
      [`(cdr ,e)
       (match (valof e env)
         [`(,ls ,env)
          `(,(cdr ls) ,env)])]
      [`(cons ,a ,d)
       (match (valof a env)
         [`(,a ,env)
          (match (valof d env)
            [`(,d ,env)
             `((,a . ,d)
               ,env)])])]
      [`(if ,test ,then ,else)
       (match (valof test env)
         [`(,test ,env)
          (if test
              (valof then env)
              (valof else env))])]
      [`(λ (,x) ,body)
       `(,(λ (arg env)
            (valof body (λ (y) (if (eqv? y x) arg (env y)))))
         ,env)]
      [`(,rator ,rand)
       (match (valof rator env)
         [`(,clos ,env)
          (match (valof rand env)
            [`(,a ,env)
             (clos a env)])])])))


(car (valof
   '(((λ (map)
        (λ (ls)
          (((map map) (λ (x) (cons x ls))) ls))
        )
      (λ (map)
        (λ (f)
          (λ (ls)
            (if (null? ls)
                '()
                (cons (f (car ls))
                      (((map map) f) (cdr ls))))))))
     '(1 2 3 4))
   '()))

