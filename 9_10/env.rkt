#lang racket

(define table
  (λ (y)
    (match y
      [`a 'ant]
      [`b 'boar]
      [`c 'cat]
      [`d 'elephant])))

#|representing environment with functions|#
#;
(define ext-env
  (λ (env x val)
    (λ (y)
      (if (eqv? y x)
          val
          (env y)))))

#;
(define apply-env
  (λ (env y)
    (env y)))

#;
(define init-env
  (λ (y)
    (error "no val for" y)))

#|representing environment with lists|#
(define apply-env
  (λ (env y)
    (match env
      [`() (error "no val for" y)]
      [`((,x . ,val) . ,env^)
       (if (eqv? y x)
          val
          (apply-env env^ y))])))
(define ext-env
  (λ (env^ x val)
    `((,x . ,val) . ,env^)))
(define init-env
  '())


#;
(define table₁
  (ext-env (ext-env (ext-env (ext-env (ext-env (ext-env init-env 'd 'elephant)
                                               'c 'cat)
                                      'b 'boar)
                             'a 'ant)
                    'e 'dog)
           'f 'frog))

