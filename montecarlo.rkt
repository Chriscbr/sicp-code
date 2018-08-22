#lang sicp
(#%require r5rs/init)

(define (rand) (random 1000000000))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials 
                          cesaro-test))))
(define (cesaro-test)
   (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) 
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) 
                 trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral pred x1 x2 y1 y2 trials)
  (define total-area
    (abs (* (- x2 x1) (- y2 y1))))
  (define fraction
    (monte-carlo
     trials
     (lambda ()
         (let ((x (random-in-range x1 x2))
               (y (random-in-range y1 y2)))
           (pred x y)))))
  (* total-area fraction))

; ex.
(define (circle x y) (< (+ (* x x) (* y y)) 1))
(estimate-integral circle -1.0 1.0 -1.0 1.0 1000)