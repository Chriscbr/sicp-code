#lang sicp
(#%require r5rs/init)

; stream helpers

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc 
                    (map stream-cdr 
                         argstreams))))))

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

(define (scale-stream stream factor)
  (stream-map
   (lambda (x) (* x factor))
   stream))

(define (partial-sums stream)
  (cons-stream (stream-car stream)
               (add-streams (stream-cdr stream)
                            (partial-sums stream))))

(define (add-streams s1 s2) 
  (stream-map + s1 s2))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (square x) (* x x))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) 
         the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream 
          (stream-car stream)
          (stream-filter 
           pred
           (stream-cdr stream))))
        (else (stream-filter 
               pred 
               (stream-cdr stream)))))

; sqrt with newton's method

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 
     1.0 (stream-map
          (lambda (guess)
            (sqrt-improve guess x))
          guesses)))
  guesses)

; estimating pi

(define (pi-summands n)
  (cons-stream 
   (/ 1.0 n)
   (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream 
   (partial-sums (pi-summands 1)) 4))

; accelerated sequences

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))     ; Sn-1
        (s1 (stream-ref s 1))     ; Sn
        (s2 (stream-ref s 2)))    ; Sn+1
    (cons-stream 
     (- s2 (/ (square (- s2 s1))
              (+ s0 (* -2 s1) s2)))
     (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream 
   s
   (make-tableau
    transform
    (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

; tolerance

(define (stream-limit s tolerance)
  (let ((first (stream-car s))
        (second (stream-car (stream-cdr s))))
    (if (< (abs (- second first)) tolerance)
        second
        (stream-limit (stream-cdr s) tolerance))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

; ln2

(define (integers-starting-from n)
  (cons-stream 
   n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define ln2-summands
    (stream-map (lambda (x)
                  (* (/ 1 x)
                     (expt -1 (+ x 1))
                     1.0)) ; decimal for convenience
                integers))

(define ln2-stream
  (partial-sums ln2-summands))

; infinite pairs

(define (divisible? x y) (= (remainder x y) 0))

(define primes
  (cons-stream
   2 (stream-filter 
      prime? (integers-starting-from 3))))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream 
       (stream-car s1)
       (interleave s2 (stream-cdr s1)))))

; pairs (i, j) where i in s, j in t, and i < j
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) 
                  (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s)
           (stream-cdr t)))))

(define int-pairs (pairs integers integers))

(define prime-sum-pairs
  (stream-filter 
   (lambda (pair)
     (prime? (+ (car pair) (cadr pair))))
   int-pairs))

; pairs (i, j) where i in s, j in t
(define (all-pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (interleave
     (stream-map (lambda (x)
                   (list (stream-car s) x))
                 (stream-cdr t))
     (stream-map (lambda (x)
                   (list x (stream-car t)))
                 (stream-cdr s)))
    (all-pairs (stream-cdr s) (stream-cdr t)))))

; pythagorean triples

(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (x)
                  (list (stream-car s) (stream-car t) x))
                (stream-cdr u))
    (interleave
     (stream-map (lambda (x)
                   (append (list (stream-car s)) x))
                 (pairs (stream-cdr t) (stream-cdr u)))
     (triples (stream-cdr s) (stream-cdr t) (stream-cdr u))))))

(define int-triples
  (triples integers integers integers))

(define pythagorean-triples
  (stream-filter
   (lambda (triple)
     (let ((a (list-ref triple 0))
           (b (list-ref triple 1))
           (c (list-ref triple 2)))
       (= (+ (square a) (square b))
          (square c))))
   int-triples))

; weighted merging

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream 
                   s1car 
                   (merge (stream-cdr s1) 
                          s2)))
                 ((> s1car s2car)
                  (cons-stream 
                   s2car 
                   (merge s1 
                          (stream-cdr s2))))
                 (else
                  (cons-stream 
                   s1car
                   (merge 
                    (stream-cdr s1)
                    (stream-cdr s2)))))))))

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1-car (stream-car s1))
               (s2-car (stream-car s2)))
           (let ((s1-weight (weight s1-car))
                 (s2-weight (weight s2-car)))
             (cond ((<= s1-weight s2-weight)
                    (cons-stream
                     s1-car
                     (merge-weighted (stream-cdr s1) s2 weight)))
                   ((<= s2-weight s1-weight)
                    (cons-stream
                     s2-car
                     (merge-weighted (stream-cdr s2) s1 weight)))
                   (else
                    (cons-stream
                     s1-car
                     (cons-stream
                      s2-car
                      (merge-weighted (stream-cdr s1)
                                      (stream-cdr s2)
                                      weight))))))))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) 
                  (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s)
                    (stream-cdr t)
                    weight)
    weight)))

(define int-pairs-ordered-by-sum
  (weighted-pairs integers integers
                  (lambda (pair)
                    (+ (car pair) (cadr pair)))))

(define int-pairs-special
  (let ((source
         (stream-filter
          (lambda (x)
            (not (or (divisible? x 2)
                     (divisible? x 3)
                     (divisible? x 5))))
          integers)))
    (weighted-pairs source source
                    (lambda (pair)
                      (+ (* 2 (car pair))
                         (* 3 (cadr pair))
                         (* 5 (car pair) (cadr pair)))))))

; ramanujan numbers

(define (sum-cube pair)
  (+ (* (car pair) (car pair) (car pair))
     (* (cadr pair) (cadr pair) (cadr pair))))

(define sum-cube-pairs
  (weighted-pairs integers integers sum-cube))

(define (consec-pairs-eq stream weight)
  (if (= (weight (stream-ref stream 0))
         (weight (stream-ref stream 1)))
      (cons-stream
       (stream-ref stream 0)
       (cons-stream
        (stream-ref stream 1)
        (consec-pairs-eq
         (stream-cdr (stream-cdr stream))
         weight)))
      (consec-pairs-eq
       (stream-cdr stream)
       weight)))

(define ramanujan-pairs
  (consec-pairs-eq sum-cube-pairs sum-cube))

(define (sum-square pair)
  (+ (* (car pair) (car pair))
     (* (cadr pair) (cadr pair))))

(define sum-square-pairs
  (weighted-pairs integers integers sum-square))

(define (triple-consec-pairs-eq stream weight)
  (if (= (weight (stream-ref stream 0))
         (weight (stream-ref stream 1))
         (weight (stream-ref stream 2)))
      (cons-stream
       (stream-ref stream 0)
       (cons-stream
        (stream-ref stream 1)
        (cons-stream
         (stream-ref stream 2)
         (triple-consec-pairs-eq
          (stream-cdr (stream-cdr (stream-cdr stream)))
          weight))))
      (triple-consec-pairs-eq
       (stream-cdr stream)
       weight)))
