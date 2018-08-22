#lang sicp
(#%require r5rs/init)

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if (= key (cadr record))
        (cddr record)
        false)))

; finds the appropriate value OR
; where the value should be placed
(define (assoc key record)
  (if (null? record)
      false
      (let ((left-branch (caar record))
            (right-branch (cdar record)))
        (cond ((= key (cadr record))
               record)
              ((and (null? left-branch)
                    (null? right-branch))
               record)
              ((null? left-branch)
               (assoc key right-branch))
              ((null? right-branch)
               (assoc key left-branch))
              ((> key (cadr record))
               (assoc key right-branch))
              ((< key (cadr record))
               (assoc key left-branch))))))

(define (insert! key value table)
  (let ((new-val (cons (cons '() '())
                       (cons key value)))
        (record (assoc key (cdr table))))
    (cond ((not record) ; table is empty
           (set-cdr! table new-val))
          ((= key (cadr record))
           (set-cdr! record (cons key value)))
          ((< key (cadr record))
           (set-car! (car record) new-val))
          (else
           (set-cdr! (car record) new-val)))))

(define (make-table)
  (list '*table*))