#lang sicp
(#%require r5rs/init)

(define (eval e env)
  (cond ((symbol? e)
         (cadr (assq e env)))
        ((eq? (car e) 'lambda)
         (cons e env))
        (else
         (apply (eval (car e) env)
                (eval (cadr e) env)))))

; apply takes a function and an argument to a value
(define (apply f x)
  (eval (cddr (car f))
        (cons (list (cadr (car f)) x) (cdr f))))

; read and parse stdin, then evaluate:
(display (eval (read) '())) (newline)