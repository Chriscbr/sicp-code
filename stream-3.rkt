#lang sicp
(#%require r5rs/init)

; stream helpers

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      '() ; the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc 
                    (map stream-cdr 
                         argstreams))))))

(define (add-streams s1 s2) 
  (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map
   (lambda (x) (* x factor))
   stream))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin 
        (proc (stream-car s))
        (stream-for-each proc 
                         (stream-cdr s)))))

(define (display-line x)
  (newline)
  (display x))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-n stream n)
  (if (= n 0)
      'done
      (begin (display-line (stream-car stream))
             (display-n (stream-cdr stream) (- n 1)))))

(define (average a b)
  (/ (+ a b) 2))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

; signals

;(define (integral integrand initial-value dt)
;  (define int
;    (cons-stream 
;     initial-value
;     (add-streams (scale-stream integrand dt)
;                  int)))
;  int)

(define ones (cons-stream 1 ones))

(define alternating-ones
  (cons-stream 1
               (cons-stream 0
                            (cons-stream -1
                                         (cons-stream 0 alternating-ones)))))

(define (rc R C dt)
  (lambda (i v0) ; current (stream), initial voltage (value)
    (add-streams
     (scale-stream i R)
     (integral (scale-stream i (/ 1 C)) v0 dt))))

(define rc1 (rc 5 1 0.5))

(define (sign-change-detector a b)
  (cond ((and (>= a 0) (< b 0)) 1)
        ((and (< a 0) (>= b 0)) -1)
        (else 0)))

(define sense-data alternating-ones)

(define zero-crossings
  (stream-map sign-change-detector 
              sense-data 
              (cons-stream 0 sense-data)))

(define (make-zero-crossings 
         input-stream last-value last-avpt)
  (let ((avpt 
         (/ (+ (stream-car input-stream) 
               last-value) 
            2)))
    (cons-stream 
     (sign-change-detector avpt last-avpt)
     (make-zero-crossings 
      (stream-cdr input-stream) (stream-car input-stream) avpt))))

(define (smooth stream)
  (cons-stream (average (stream-ref stream 0)
                        (stream-ref stream 1))
               (smooth (stream-cdr stream))))

; delayed evaluation

(define (integral
         delayed-integrand initial-value dt)
  (define int
    (cons-stream 
     initial-value
     (let ((integrand 
            (force delayed-integrand)))
       (add-streams 
        (scale-stream integrand dt)
        int))))
  int)

; workaround from github.com/crowding/sicp/blob/master/chap3.5.rkt

(define (solve f y0 dt)
  (let ((y nil) (dy nil))
    (set! y (integral (delay dy) y0 dt))
    (set! dy (stream-map f y))
    y))

(define e-const (stream-ref (solve (lambda (y) y) 1 0.001) 1000))

; rand-update from https://github.com/trptcolin/sicp-study/blob/master/scheme/week-8/exercise-3.6.scm

(define (rand-update x)
  (let ((a 16807) (b 0) (m 2147483647))
    (modulo (+ (* a x) b) m)))

(define random-init (rand-update 1))

(define random-numbers
  (cons-stream random-init
               (stream-map rand-update 
                           random-numbers)))

(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) 
      (stream-car (stream-cdr s)))
   (map-successive-pairs 
    f (stream-cdr (stream-cdr s)))))

(define cesaro-stream
  (map-successive-pairs
   (lambda (r1 r2) (= (gcd r1 r2) 1))
   random-numbers))

(define (monte-carlo experiment-stream 
                     passed 
                     failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) 
      passed 
      failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define pi
  (stream-map
   (lambda (p) (sqrt (/ 6 p)))
   (monte-carlo cesaro-stream 1 0))) ; start with one pass to prevent div by 0