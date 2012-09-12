#lang plai
(define-type MisspelledAnimal
  [caml (humps number?)]
  [yacc (height number?)])
(define ma1 (caml 2))
(define ma2 (yacc 1.9))
(define (good? ma)
  (cond
    [(caml? ma) (>= (caml-humps ma) 2)]
    [(yacc? ma) (> (yacc-height ma) 2.1)]))
(test (good? ma1) #t)
(test (good? ma2) #f)

(define v '(+ 5 (* 1 2)))
(test (list? v) #t)

(define-type ArithC
  [numC (n number?)]
  [plusC (l ArithC?) (r ArithC?)]
  [multC (l ArithC?) (r ArithC?)])
(define (parse e)
  (cond
    [(number? e) (numC e)]
    [(list? e)
     (case (first e)
       [(+) (plusC (parse (second e)) (parse (third e)))]
       [(*) (multC (parse (second e)) (parse (third e)))])]))

(define-type FunDefC
  [fdC (name symbol?) (arg symbol?) (body <exprC>)])
(define-type ExprC
  [numC (n number?)]
  [idC (s symbol?)]
  [appC (fun symbol?) (arg ExprC?)]
  [plusC (l ExprC?) (rExprC?)]
  [multC (l ExprC?) (rExprC?)])

(define (interp a)
  (type-case ArithC a
    [numC (n) n]
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]))
(define i (interp (parse v)))
(test i 7)

(define-type ArithS
  [numS (n number?)]
  [plusS (l ArithS?) (r ArithS?)]
  [bminusS (l ArithS?) (r ArithS?)]
  [uminusS (e ArithS?)]
  [multS (l ArithS?) (r ArithS?)])

(define (desugar as)
  (type-case ArithS as
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l)
                        (desugar r))]
    [multS (l r) (multC (desugar l)
                        (desugar r))]
    [uminusS (e) (desugar (bminusS (numS 0) e))]
    [bminusS (l r) (plusC (desugar l)
                          (multC (numC -1) (desugar r)))]))