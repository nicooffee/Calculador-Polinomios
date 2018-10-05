#lang racket
(define (value A) (when (pair? A) (car A)))
(define (left A) (when (pair? A)(cadr A)))
(define (right A) (when (pair? A)(caddr A)))

(define (split L)
  (define (cortar L s n)
    (cond
      ((null? L) L)
      ((> n 0) (cortar (cdr L) (append s (list (car L))) (- n 1)))
      (else (list s L))
      )
    )
  (let
      ((mitad (quotient (length L) 2))
       )
    (cortar L (list) mitad)
    )
  )

(define (createAbb L)
  (cond
    ((null? L) L)
    ((= (length L) 1) (list (car L) (list) (list)))
    (else
     (let
         ((splitL (split L))
          )
       (list (car (cadr splitL))(createAbb (car splitL)) (createAbb (cdr (cadr splitL))))
       )
     )
    )
  )
(createAbb (list 2 3 4 6 9 ))