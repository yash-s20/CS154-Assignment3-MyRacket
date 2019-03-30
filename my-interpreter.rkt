#lang racket
(require racket/struct)
(provide (all-defined-out))
(require "defs.rkt")
(require "examples.rkt")

;;Global definitions. A counter that tracks the framenumber
(define framenumber 0)

;The stack and its operations. I have decided to make the stack a global.
(define stack '())
(define (push frame) (set! stack (cons frame stack)))
(define (pop) (if (null? stack) (error "Empty stack")
                        (set! stack (cdr stack))))
(define (top) (if (null? stack) (error "Empty stack")
                        (car stack)))


;createframe creates a new frame. It gets its number from
;;framenumber and as a side effect increases framenumber
(define (createframe hashtable parent) ;hastable gives the initial bindings
   (let ([fn framenumber])
     (begin
       (set! framenumber (+ 1 framenumber))
       (frame fn hashtable parent))))

;This creates the global frame, which, in our case, contains
;empty bindings.
(push (createframe (make-hash '()) (emptyframe)))

;This interprets a program. It uses the following function processdef.
(define (eval-program prog)
         (match prog
           [(pgm deflist)
            (begin
              (map (lambda (def) (processdef def (top))) deflist)
              (return-value-of-main (top)))]))

;;processdef describes how each definition is processed and added to
;;the frame fr.
(define (processdef defn fr)
  (match defn    
    [(def v/f exp) (hash-set! (frame-bindings fr) v/f (eval-exp exp))]))

;; We have said that the result of the program is the value of
;; the symbol main. main must be a defined symbol in each program.
(define (return-value-of-main frame)
  (hash-ref! (frame-bindings frame) 'main "main not found"))

;; The expression evaluator is the heart of the interpreter.
;; It will use the functions below
(define (eval-exp exp)
  (cond [(boolean? exp) exp]
        [(number? exp) exp]
        [(list? exp) exp]
        [(string? exp) exp]
        [(symbol? exp) (match (search exp (top))
                         [(emptyframe) (error "Symbol not found")]
                         [(frame _ b _) (hash-ref b exp)])]
        [else (match exp
                [(uexp op exp1) (op (eval-exp exp1))]
                [(bexp op exp1 exp2) (op (eval-exp exp1) (eval-exp exp2))]
                [(lam var explam) (closure exp (top))]
                [(app exp1 explist) (match (eval-exp exp1)
                                      [(closure lmbd fr)
                                       (match lmbd
                                         [(lam var explam)
                                          ((lambda (binds)
                                             (begin
                                               (push (createframe (make-hash (map (lambda (x y) (cons x (eval-exp y))) var binds)) fr))
                                               (let ([ret (eval-exp explam)])
                                                 (begin
                                                   (pop)
                                                   ret)))) explist)])])]
                [(beginexp explist) (process-beginexp explist)]
                [(sett v/f exp) (hash-set! (frame-bindings (top)) v/f (eval-exp exp))]
                [(lett deflist exp) (process-lets deflist exp)]
                [(debugexp) (begin
                              (print-current-environment (top))
                              )]
                [(iff boolexp exp1 exp2)
                 (if (eval-exp boolexp) (eval-exp exp1) (eval-exp exp2))]
                [(lets deflist exp) (process-letss deflist exp (top))]
                [(defexp deflist exp) (begin
                                        (map (lambda(def) (processdef def (top))) deflist)
                                        (eval-exp exp))]
                )]))

  
;;An auxilliary function that processes a begin expression
(define (process-beginexp explist)
  ;(match expllist
  (last (map eval-exp explist)))

;;An auxilliary function that processes a let expression.
;;The let definitions are in deflist, and the let body is exp.
(define (process-lets deflist exp)
  (let ([curr-fr (createframe (make-hash '()) (top))])
    (begin
      (map (lambda(def) (processdef def curr-fr)) deflist)
      (push curr-fr)
      (let ([ret (eval-exp exp)])
        (begin
          (pop)
          ret)))))

(define (process-letss deflist exp fr)
  (match deflist
    ['() (begin
           (push fr)
           (let ([ret (eval-exp exp)])
             (begin
               (pop)
               ret)))]
    [(cons a b)
     (begin
       (push fr)
       (let ([curr-fr (createframe (make-hash '()) fr)])
         (begin
           (processdef a curr-fr)
           (let ([ret (process-letss b exp curr-fr)])
             (begin
               (pop)
               ret)))))]))
;;Prints the current environment running through a chain of frames.
;;Note that my struct definitions are such that if fr is a frame,
;;then (displayln fr) will print the frame in the format that I have
;;shown. 
(define (print-current-environment fr)
  (match fr
    [(emptyframe) (begin
                    (displayln "@@@@@@@@@@@@@@@@@@@@@@@")
                    (displayln fr))]
    [(frame _ _ (emptyframe)) (begin
                                (displayln "@@@@@@@@@@@@@@@@@@@@@@@")
                                (displayln fr)
                                (displayln "@@@@@@@@@@@@@@@@@@@@@@@"))]
    [(frame _ _ p) (begin
                     (displayln "@@@@@@@@@@@@@@@@@@@@@@@")
                     (displayln fr)
                     (print-current-environment p))]))

;;Search for the symbol sym in an environment that starts with the frame
;;fr. We shall make search return either the  emptyframe
;;or the frame containing the symbol (note, not the value of the
;;symbol itself.

(define (search sym fr)
  (match fr
    [(emptyframe) fr]
    [(frame _ b p)
     (if (hash-has-key? b sym)
              fr (search sym p))]))

