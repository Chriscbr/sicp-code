#lang sicp

; code from MIT 6.001 Spring 2005 Project, objsys.scm

; helper

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate
                       (cdr sequence))))
        (else (filter predicate
                      (cdr sequence)))))

;;------------------------------------------------------------
;; Instance

; instance is a tagged data structure which holds the "self" of a normal
; object instance.  It passes all messages along to the handler
; procedure that it contains.
;

(define (make-instance)
  (list 'instance #f))

(define (instance? x)
  (and (pair? x) (eq? (car x) 'instance)))

(define (instance-handler instance) (cadr instance))

(define (set-instance-handler! instance handler)
  (set-car! (cdr instance) handler))

(define (create-instance maker . args)
  (let* ((instance (make-instance))
         (handler (apply maker instance args)))
    (set-instance-handler! instance handler)
    (if (method? (get-method 'INSTALL instance))
        (ask instance 'INSTALL))
    instance))

;;------------------------------------------------------------
;; Handler
; handler is a procedure which responds to messages with methods
; it automatically implements the TYPE and METHODS methods.

(define (make-handler typename methods . super-parts)
  (cond ((not (symbol? typename))    ;check for possible programmer errors
	 (error "bad typename" typename))
	((not (method-list? methods))
	 (error "bad method list" methods))
	((and super-parts (not (filter handler? super-parts)))
	 (error "bad part list" super-parts))
	(else
	 (lambda (message)
	   (case message
	     ((TYPE)
	      (lambda () (type-extend typename super-parts)))
	     ((METHODS)
	      (lambda ()
		(append (method-names methods)
			(append-map (lambda (x) (ask x 'METHODS))
				    super-parts))))
	     (else
	      (let ((entry (method-lookup message methods)))
		(if entry
		    (cadr entry)
		    (find-method-from-handler-list message super-parts)))))))))

(define (handler? x)
  (and (compound-procedure? x)
       (eq? 'handler (lambda-name (procedure-lambda x)))))

(define (->handler x)
  (cond ((instance? x)
	 (instance-handler x))
	((handler? x)
	 x)
	(else
	 (error "I don't know how to make a handler from" x))))

; builds a list of method (name,proc) pairs suitable as input to make-handler
; note that this puts a label on the methods, as a tagged list

(define (make-methods . args)
  (define (helper lst result)
    (cond ((null? lst) result)

	  ; error catching
	  ((null? (cdr lst))
	   (error "unmatched method (name,proc) pair"))
	  ((not (symbol? (car lst)))
	   (if (procedure? (car lst))
	       (pp (car lst)))
	   (error "invalid method name" (car lst)))
	  ((not (procedure? (cadr lst)))
	   (error "invalid method procedure" (cadr lst)))

	  (else
	   (helper (cddr lst) (cons (list (car lst) (cadr lst)) result)))))
  (cons 'methods (reverse (helper args '()))))

(define (method-list? methods)
  (and (pair? methods) (eq? (car methods) 'methods)))

(define (empty-method-list? methods)
  (null? (cdr methods)))

(define (method-lookup message methods)
  (assq message (cdr methods)))

(define (method-names methods)
  (map car (cdr methods)))

;;------------------------------------------------------------
;; Root Object

; Root object.  It contains the IS-A method.
; All classes should inherit (directly or indirectly) from root.
;
(define (root-object self)
  (make-handler
   'root
   (make-methods
    'IS-A
    (lambda (type)
      (memq type (ask self 'TYPE))))))


;;------------------------------------------------------------
;; Object Interface

; ask
; 
; We "ask" an object to invoke a named method on some arguments.
;
(define (ask object message . args)
  ;; See your Scheme manual to explain `. args' usage
  ;; which enables an arbitrary number of args to ask.
  (let ((method (get-method message object)))
    (cond ((method? method)
           (apply method args))
          (else
           (error "No method for" message 'in 
                  (safe-ask 'UNNAMED-OBJECT
                            object 'NAME))))))

; Safe (doesn't generate errors) method of invoking methods
; on objects.  If the object doesn't have the method,
; simply returns the default-value.  safe-ask should only
; be used in extraordinary circumstances (like error handling).
;
(define (safe-ask default-value obj msg . args)
  (let ((method (get-method msg obj)))
    (if (method? method)
        (apply ask obj msg args)
        default-value)))


;;--------------------
;; Method Interface
;;
;; Objects have methods to handle messages.

; Gets the indicated method from the object or objects.
; This procedure can take one or more objects as 
; arguments, and will return the first method it finds 
; based on the order of the objects.
;
(define (get-method message . objects)
  (find-method-from-handler-list message (map ->handler objects)))

(define (find-method-from-handler-list message objects)
  (if (null? objects)
      (no-method)
      (let ((method ((car objects) message)))
	(if (not (eq? method (no-method)))
	    method
	    (find-method-from-handler-list message (cdr objects))))))

(define (method? x)
  (cond ((procedure? x) #T)
        ((eq? x (no-method)) #F)
        (else (error "Object returned this non-message:" x))))

(define no-method
  (let ((tag (list 'NO-METHOD)))
    (lambda () tag)))

; used in make-handler to build the TYPE method for each handler

(define (type-extend type parents)
  (cons type 
        (remove-duplicates
         (append-map (lambda (parent) (ask parent 'TYPE))
                     parents))))
