#lang sicp

(define (inc x) (+ x 1))

(define (cube x) (* x x x))

(define (even? x) (= (remainder x 2) 0))

(define (identity x) x)

; recursive version
(define (sum-rec term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum-rec term (next a) next b))))

; iterative version
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

(define (simpsons-integral f a b n)
  (define h (/ (- b a) n))
  (define (scale num)
    (cond ((or (= num 0) (= num n)) 1)
          ((even? num) 2)
          (else 4)))
  (define (y k)
    (* (f (+ a
             (* k h)))
       (scale k)))
  (/ (* h
        (sum y 0 inc n))
     3.0))

; recursive version
(define (product-rec term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-rec term (next a) next b))))

; iterative version
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (product identity 1 inc n))

(define (pi-product n)
  (define (num x)
    (if (even? x)
        (+ x 2)
        (+ x 1)))
  (define (denom x)
    (if (even? x)
        (+ x 1)
        (+ x 2)))
  (/ (product num 1 inc n)
     (product denom 1 inc n)))

(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-rec combiner null-value term (next a) next b))))

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (sum-acc term a next b)
  (accumulate + 0 term a next b))

(define (product-acc term a next b)
  (accumulate * 1 term a next b))

(define (filtered-accumulate combiner null-value term a next b pred)
  (define (iter a result)
    (cond ((> a b) result)
          ((pred a)
           (iter (next a) (combiner (term a) result)))
          (else
           (iter (next a) result))))
  (iter a null-value))