#lang sicp
(#%require r5rs/init)

; prereq functions

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate
                       (cdr sequence))))
        (else (filter predicate
                      (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low
            (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

; main procedures

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) 
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position 
                    new-row 
                    k 
                    rest-of-queens))
                 (enumerate-interval 
                  1 
                  board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board (list))

(define (last-item items)
  (if (null? (cdr items))
      (car items)
      (last-item (cdr items))))

(define (reverse items)
  (define (iter old new)
    (if (null? old)
        new
        (iter (cdr old) (cons (car old) new))))
  (iter items nil))

(define (all-but-last items)
  (if (null? items)
      nil
      (reverse (cdr (reverse items)))))

(define (safe? k positions)
  (define (contains? items value)
    (cond ((null? items) false)
          ((= value (car items)) true)
          (else (contains? (cdr items) value))))
  (define (safe-diagonals? items value)
    (let ((diffs (map (lambda (x) (- k x))
                      (enumerate-interval 1 (- k 1)))))
      (define (iter iter-items iter-diffs)
        (if (null? iter-items)
            true
            (and (not (or (= (car iter-items) (- (last-item positions) (car iter-diffs)))
                          (= (car iter-items) (+ (last-item positions) (car iter-diffs)))))
                 (iter (cdr iter-items) (cdr iter-diffs)))))
      (iter items diffs)))
  (and (not (contains? (all-but-last positions) (last-item positions)))
       (safe-diagonals? (all-but-last positions) (last-item positions))))
      

(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list new-row)))