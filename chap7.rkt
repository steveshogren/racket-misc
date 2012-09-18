#lang plai-typed
(define-type Binding
  [mt]
  [bind (name : symbol) (val : Value)])
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)
(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define (get-fundef n fds)
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined function")]
    [(cons? fds) (cond
                   [(symbol=? n (fdC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))


(define-type Value
  [numV (n : number)]
  [funV (name : symbol) (arg : symbol) (body : ExprC)])

(define (interp [expr : ExprC] [env : Env]) : Value
  (type-case ExprC expr
    [numC (n) (numV n)]
    [idC (n) (lookup n env)]
    [fdC (n a b) (funV n a b)]
    [appC (f a) (local ([define fd (interp f env)])
              (interp (funV-body fd)
                      (extend-env (bind (funV-arg fd)
                                        (interp a env))
                                  mt-env)))]
    [plusC (l r) (num+ (interp l env) (interp r env))]
    [multC (l r) (num* (interp l env) (interp r env))]))
(define (num+ [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (+ (numV-n l) (numV-n r)))]
    [else
     (error 'num+ "one argument was not a number")]))
(define (num* [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (* (numV-n l) (numV-n r)))]
    [else
     (error 'num+ "one argument was not a number")]))

(define (lookup [for : symbol] [env : Env]) : number
  (cond
    [(empty? env) (error 'lookup "name not found")]
    [else (cond
            [(symbol=? for (bind-name (first env)))
             (bind-val (first env))]
            [else (lookup for (rest env))])]))


(test (interp (plusC (numC 10) (appC (fdC 'const5 '_ (numC 5)) (numC 10)))
              mt-env)
      (numV 15))
 
(test/exn (interp (appC (fdC 'f1 'x (appC (fdC 'f2 'y (plusC (idC 'x) (idC 'y)))
                                          (numC 4)))
                        (numC 3))
                  mt-env)
          "name not found")