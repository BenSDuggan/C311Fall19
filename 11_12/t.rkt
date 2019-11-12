#lang racket
(require "mk.rkt")

(define (lookup env x)
  (cond
    [(null? env) (error "oops")]
    [else (match-let ([`((,y . ,v) . ,d) env])
            (if (eqv? y x)
                v
                (lookup d x)))]))

(defrel (lookupᵒ Γ x o)
  (condᵉ
   #;
   [(== '() env) (== #t #f)]
   [(fresh (y t Γ^)
      (== `((,y . ,t) . ,Γ^) Γ)
      (condᵉ
       [(== y x) (== t o)]
       [(=/= y x) (lookupᵒ Γ^ x o)]))]))
#|
 Γ ⊢ e : τ
 how to pronounce:
 in gamma, we can infer e has type tau
|#

(defrel (⊢ Γ e τ)
  (condᵉ
   [(numberᵒ e) (== τ 'Nat)]
   [(== τ 'Bool)
    (condᵉ
     [(== #f e)]
     [(== #t e)])]
   [(fresh (nexp)
      (== `(zero? ,nexp) e)
      (== τ 'Bool)
      (⊢ Γ nexp 'Nat))]
   [(fresh (nexp)
      (== `(sub1 ,nexp) e)
      (== τ 'Nat)
      (⊢ Γ nexp 'Nat))]
   [(fresh (nexp₁ nexp₂)
      (== `(+ ,nexp₁ ,nexp₂) e)
      (== τ 'Nat)
      (⊢ Γ nexp₁ 'Nat)
      (⊢ Γ nexp₂ 'Nat))]
   [(fresh (nexp₁ nexp₂)
      (== `(* ,nexp₁ ,nexp₂) e)
      (== τ 'Nat)
      (⊢ Γ nexp₁ 'Nat)
      (⊢ Γ nexp₂ 'Nat))]
   [(fresh (test conseq alter)
      (== `(if ,test ,conseq ,alter) e)
      (⊢ Γ test 'Bool)
      (⊢ Γ conseq τ)
      (⊢ Γ alter τ))]
   [(symbolᵒ e)
    (lookupᵒ Γ e τ)]
   [(fresh (x body)
      (== `(λ (,x) ,body) e)
      (fresh (τx τbody)
        (== `(→ ,τx ,τbody) τ)
        (⊢ `((,x . ,τx) . ,Γ) body τbody)))]
   [(fresh (rator rand)
      (== `(,rator ,rand) e)
      (fresh (τx τbody)
        (⊢ Γ rator `(→ ,τx ,τbody))
        (⊢ Γ rand τx)
        (== τbody τ)))]
   [(fresh (x body)
      (== `(fix (λ (,x) ,body)) e)
      (⊢ `((,x . ,τ) . ,Γ) body τ))]))

#;
(run 1 τ
  (⊢ '()
   `(fix (λ (fact)
          (λ (n)
            (if (zero? n)
                1
                (* n (fact (sub1 n)))))))
   τ))

(run 1 τ
  (⊢ '()
     `((fix (λ (fact)
               (λ (n)
                 (* n (fact (sub1 n))))))
       5)
   τ))

#;
(run 1 τ
  #|self-application doesn't have a type|#
  (⊢ '() '(λ (x) (x x)) τ))
