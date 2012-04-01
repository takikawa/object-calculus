#lang racket

;; Programming with the object calculus

(require (for-syntax racket syntax/parse))

(begin-for-syntax
  ;; object elements
  ;; (l = (sigma (this) this))
  (define-syntax-class label-sigma-pair
    (pattern (l:id (~datum =) s:sigma-expression)
             #:with label #'l
             #:with meth #'s.meth))
  
  ;; methods/fields
  (define-syntax-class sigma-expression
    #:description "sigma expression"
    (pattern ((~literal sigma) (self:id) body:expr)
             #:with meth #'(lambda (self) body))))

(define-syntax (object stx)
  (syntax-parse stx
    [(_ p:label-sigma-pair ...)
     (let ([key+vals (flatten (map list
                                   (syntax->list #'('p.l ...))
                                   (syntax->list #'(p.meth ...))))])
       #`(hash #,@key+vals))]))

;; Object calculus operations
(define-syntax (select stx)
  (syntax-parse stx
    [(_ obj label:id)
     #'((hash-ref obj 'label)
        obj)]))

(define-syntax (update stx)
  (syntax-parse stx
    [(_ obj label:id s:sigma-expression)
     #'(hash-set obj 'label s.meth)]))

;; Encodings of lambda and application
(define-syntax (my-lambda stx)
  (syntax-parse stx
    [(_ (x:id) body)
     #'(object [val = (sigma (s) s)]
               [apply = (sigma (s)
                          (let-syntax ([x (syntax-id-rules () [x (select s val)])])
                            body))])]))

(define-syntax (my-app stx)
  (syntax-parse stx
    [(_ f arg)
     #'(select (update f val (sigma (s) arg))
               apply)]))

;; Traits & classes
(define-syntax (trait stx)
  (syntax-parse stx
    [(_ (l:id (~datum =) f) ...)
     #'(object [l = (sigma (s) f)] ...)]))

(define-syntax (class stx)
  (syntax-parse stx
    [(_ (l:id (~datum =) f) ...)
     #'(object
        [new = (sigma (s)
                 (object [l = (sigma (s1) ((select (select s pre) l) s1))] ...))]
        [pre = (sigma (s) (trait [l = f] ...))])]))

;; Tests
(module+ test
  (require rackunit)
  (define o (object [x = (sigma (s) 5)]
                    [bump = (sigma (s)
                              (update s x (sigma (s) (+ (select o x) 1))))]))
  (check-equal? (select o x) 5)
  (check-equal? (select (select o bump) x) 6)
  
  (check-equal? (my-app (my-lambda (x) (+ x x)) 3) 6)
  
  (define point-class
    (class
      [x = (λ (s) 0)]
      [bump = (λ (s) (update s x (sigma (s) (+ (select o x) 1))))]))
  
  (check-equal? (select (select point-class new) x) 0))

