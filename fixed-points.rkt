#lang sicp
(#%require (only racket/base error))

(define (square x) (* x x))

(define (cube x) (* x x x))

(define (average a b)
  (/ (+ a b) 2))

(define (close-enough? a b)
  (< (abs (- a b)) 0.001))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (positive? a-value) (negative? b-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (cont-frac n d k)
  (define (loop prev count)
    (if (= count 0)
          prev
          (loop (/ (n count)
                   (+ (d count) prev))
                (- count 1))))
  (loop 0 k))

(define (cont-frac-rec n d k)
  (define (loop count)
    (if (> count k)
        0
        (/ (n count)
           (+ (d count)
              (loop (+ count 1))))))
  (loop 1))

(+ 2 (cont-frac (lambda (i) 1.0)
                (lambda (i)
                  (if (= (remainder i 3) 2)
                      (* 2 (/ (+ i 1) 3))
                      1.0))
                10))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= i 1)
                   x
                   (- (* x x))))
             (lambda (i) (+ -1 (* 2 i)))
             k))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (cubic a b c)
  (lambda (x)
    (+ (* x x x)
       (* a x x)
       (* b x)
       c)))

(define (double f)
  (lambda (x)
    (f (f x))))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (define (loop g k)
    (compose f g))
  (loop f n))

(define (repeated-rec f n)
  (if (< n 1)
      (lambda (x) x)
      (compose f (repeated-rec f (- n 1)))))

; dx defined previously
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

(define (n-smooth f n)
  ((repeated smooth n) f))

(define (iterative-improve good-enough? improve-guess)
  (lambda (guess)
    (let ((next (improve-guess guess)))
      (if (good-enough? guess next)
          (average guess next)
          ((iterative-improve good-enough? improve-guess) next)))))

(define (sqrt-ii n)
  ((iterative-improve close-enough?
                     (lambda (x) (average x (/ n x)))) 1.0))

(define (fixed-point-ii f first-guess)
  ((iterative-improve close-enough? f) first-guess))