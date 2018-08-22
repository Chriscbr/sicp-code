#lang sicp
(#%require r5rs/init)

(define (lookup keys table)
  (let ((record (assoc (car keys) (cdr table))))
    (if record
        (if (null? (cdr keys))
            (cdr record)
            (if (pair? (cdr record))
                (lookup (cdr keys) record)
                false))
        false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) 
         (car records))
        (else (assoc key (cdr records)))))

(define (insert! keys value table)
  (let ((record (assoc (car keys) (cdr table))))
    (if record
        (if (null? (cdr keys))
            (set-cdr! record value)
            (insert! (cdr keys) value record))
            ; this consequent will allow overwriting single
            ; table values with a subtable if needed
            ; but I don't like this solution
            ; since it assumes any pair must be part of the
            ; table structure
            ;(if (pair? (cdr record))
            ;    (insert! (cdr keys) value record)
            ;    (begin
            ;      (set-cdr! record
            ;                (cons (cons (cadr keys) '()) '()))
            ;      (insert! (cdr keys) value record))))
        (set-cdr!
         table
         (cons (unflatten keys value)
               (cdr table)))))
  'ok)

; convert
; (a), 3 to (a . 3)
; (a b), 3 to (a (b . 3))
; (a b c), 3 to (a (b (c . 3)))
(define (unflatten keys value)
  (if (null? (cdr keys))
      (cons (car keys) value)
      (list (car keys)
            (unflatten (cdr keys) value))))

(define (make-table)
  (list '*table*))