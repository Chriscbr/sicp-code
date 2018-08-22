#lang sicp
(#%require (only racket random)) ;import random

(define (square n) (* n n))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (define (next n)
    (cond ((= n 2) 3)
          (else (+ n 2))))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fermat-test-complete n)
  (define (try-it a)
    (= (expmod a n n) a))
  (define (loop count)
    (cond ((= count n) "prime")
          ((try-it count) (loop (+ count 1)))
          (else "not prime")))
  (loop 1))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

; timed tests

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end)
  (define (search-iter start end)
    (if (<= start end)
        (timed-prime-test start))
    (if (<= start end)
        (search-for-primes (+ start 2) end)))
  (search-iter (if (even? start) (+ start 1) start)
               (if (even? end) (- end 1) end)))