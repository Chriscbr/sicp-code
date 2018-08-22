#lang sicp

; code adapted from MIT 6.001, Spring 2005 project

; helpers

(define (filter predicate lst)
  (cond ((null? lst) '())
        ((predicate (car lst))
         (cons (car lst) (filter predicate (cdr lst))))
        (else (filter predicate (cdr lst)))))

(define (remove-duplicates lst)
  (if (null? lst)
      '()
      (cons (car lst)
            (remove-duplicates (filter (lambda (x) 
                                         (not (eq? x (car lst))))
                                       lst)))))

(define (append-map proc lists)
  (if (null? lists)
      lists
      (append (apply proc (car lists))
              (append-map proc (cdr lists)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (contains? list x)
  (not (eq? (memq x list) #f)))

; internal implementation

(define (make-instance)
  (let ((handler #f))
    (lambda (message)
      (case message
        ((SET-HANDLER!)
         (lambda (handler-proc) (set! handler handler-proc)))
        (else (get-method message handler))))))

(define (create-instance maker . args)
  (let* ((instance (make-instance))
         (handler (apply maker instance args)))
    (ask instance 'SET-HANDLER! handler)
    instance))

(define (get-method message . objects)
  (if (null? objects)
      (no-method)
      (let ((method ((car objects) message)))
        (if (method? method)
            method
            (apply get-method message (cdr objects))))))

(define (ask object message . args)
  (let ((method (get-method message object)))
    (if (method? method)
        (apply method args)
        (error "No method for message" message))))

(define (method? x)
  (cond ((procedure? x) #t)
        ((eq? x (no-method)) #f)
        (else (error "Object returned this non-message:" x))))

(define no-method
  (let ((tag (list 'NO-METHOD)))
    (lambda () tag)))

; type (symbol), list of object handlers -> list of types
; inner lambda function is proc -> list of types
(define (type-extend type . parents)
  (cons type
        (remove-duplicates
         (flatmap (lambda (parent) (ask parent 'TYPE)) parents))))

; root-object class

(define (make-root-object self)
  (lambda (message)
    (case message
      ((TYPE) (lambda () '(root)))
      ((IS-A) (lambda (type) (contains? (ask self 'TYPE) type)))
      (else (no-method)))))

; named-object class

(define (make-named-object self name)
  (let ((root-part (make-root-object self)))
    (lambda (message)
      (case message
        ((TYPE) (lambda ()
                  (type-extend 'named-object root-part)))
        ((NAME) (lambda () name))
        ((CHANGE-NAME) (lambda (new-name) (set! name (new-name))))
        (else (get-method message root-part))))))

(define (create-named-object name)
  (create-instance make-named-object name))

; book class

(define (make-book self name copyright)
  (let ((named-object-part (make-named-object self name)))
    (lambda (message)
      (case message
        ((TYPE) (lambda ()
                  (type-extend 'book named-object-part)))
        ((YEAR) (lambda () copyright))
        (else (get-method message named-object-part))))))

(define (create-book name copyright)
  (create-instance make-book name copyright))

; person class

(define (make-person self name)
  (let ((root-part (make-root-object self)))
    (lambda (message)
      (case message
        ((TYPE) (lambda () (type-extend 'person root-part)))
        ((WHOAREYOU?) (lambda () name))
        ((SAY) (lambda (stuff) stuff))
        (else (get-method message root-part))))))

(define (create-person name)
  (create-instance make-person name))

; professor class

(define (make-professor self name)
  (let ((person-part (make-person self name)))
    (lambda (message)
      (case message
        ((TYPE) (lambda () (type-extend 'professor person-part)))
        ((WHOAREYOU?) (lambda () (list 'prof
                                       (ask person-part 'WHOAREYOU?))))
        ((LECTURE) (lambda (stuff)
                 (cons 'therefore (ask self 'SAY stuff))))
        (else (get-method message person-part))))))

(define (create-professor name)
  (create-instance make-professor name))

; arrogant professor class

(define (make-arrogant-prof self name)
  (let ((prof-part (make-professor self name)))
    (lambda (message)
      (case message
        ((TYPE) (lambda () (type-extend 'arrogant-prof prof-part)))
        ((SAY) (lambda (stuff)
                 (append (ask prof-part 'SAY stuff) (list 'obviously))))
        (else (get-method message prof-part))))))

(define (create-arrogant-prof name)
  (create-instance make-arrogant-prof name))

; singer class

(define (make-singer self name)
  (let ((root-part (make-root-object self)))
    (lambda (message)
      (case message
        ((TYPE) (lambda () (type-extend 'singer root-part)))
        ((SAY) (lambda (stuff)
                 (append stuff '(tra la la))))
        ((SING) (lambda ()
                  (ask self 'say '(the hills are alive))))
        (else (get-method message root-part))))))

(define (create-singer name)
  (create-instance make-singer name))

; singing arrogant professor class

(define (make-singing-arrogant-prof self name)
  (let ((singer-part (make-singer self name))
        (arrogant-prof-part (make-arrogant-prof self name)))
    (lambda (message)
      (case message
        ((TYPE) (lambda () (type-extend 'singing-arrogant-prof singer-part arrogant-prof-part)))
        (else (get-method message singer-part arrogant-prof-part))))))

(define (create-singing-arrogant-prof name)
  (create-instance make-singing-arrogant-prof name))

; client code

(display 'z:) (newline)
(define z (create-book 'sicp 1996))
(ask z 'YEAR) ; 1996
(ask z 'NAME) ; sicp
(ask z 'TYPE) ; (book named-object root)
(ask z 'IS-A 'book) ; #t;
(ask z 'IS-A 'named-object) ; #t
(ask z 'IS-A 'person) ; #f
(newline)

(display 'barry:) (newline)
(define barry (create-singing-arrogant-prof 'barrington))
(ask barry 'TYPE)
(ask barry 'LECTURE '(the sky is blue))
