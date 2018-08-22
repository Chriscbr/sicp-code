#lang sicp
(#%require r5rs/init)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define vec1 (list 1 2 3))
(define vec2 (list 5 6 7))
(define mat1 (list (list 0 1 2) (list 1 1 0) (list 3 0 0)))
(define mati (list (list 1 0 0) (list 0 1 0) (list 0 0 1)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product v row)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (m-row) (matrix-*-vector cols m-row)) m)))