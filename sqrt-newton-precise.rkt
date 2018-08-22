#lang sicp

(define (cbrt x)
  ((if (< x 0)
       -
       +)
   (cbrt-iter (* (abs x) 1.0) (abs x))))

(define (cbrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (cbrt-iter (improve guess x) x)))

(define (improve guess x)
  (/ (+ (/ x
           (* guess guess))
        (* 2 guess))
     3))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) (* 0.001 x)))

(define (cube x)
  (* x x x))