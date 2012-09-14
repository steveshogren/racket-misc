#lang plai
(define-type FunDefC
  [fdC (name symbol?) (arg symbol?) (body ExprC?)])

(define-type ExprC
  [numC (n number?)]
  [idC (s symbol?)]
  [appC (fun symbol?) (arg ExprC?)]
  [plusC (l ExprC?) (r ExprC?)]
  [multC (l ExprC?) (r ExprC?)])

(define (get-fundef n fds)
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined function")]
    [(cons? fds) (cond
                   [(symbol=? n (fdC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))


(define-type Env
  [mt]
  [bind (name symbol?) (val number?) (rest Env?)])

(define (interp [expr ExprC?] [env Env?] [fds (listof FunDefC?)]) number?
  (type-case ExprC expr
    [numC (n) n]
    [idC (n) (lookup n env)]
    [appC (f a) (local ([define fd (get-fundef f fds)])
                  (interp (fdC-body fd)
                    (bind (fdC-arg fd)
                          (interp a env fds)
                          (mt))
                          fds))]
    [plusC (l r) (+ (interp l env fds) (interp r env fds))]
    [multC (l r) (* (interp l env fds) (interp r env fds))]))

(define (lookup [for symbol?] [env Env?]) number?
  (type-case Env env
    [mt () (error 'lookup "name not found")]
    [bind (n v r) (cond
                    [(symbol=? n for) v]
                    [else (lookup for r)])]))
(test (interp (plusC (numC 10) (appC 'const5 (numC 10)))
              (mt)
              (list (fdC 'const5 '_ (numC 5))))
      15)
 
(test (interp (plusC (numC 10) (appC 'double (plusC (numC 1) (numC 2))))
              (mt)
              (list (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
      16)
 
(test (interp (plusC (numC 10) (appC 'quadruple (plusC (numC 1) (numC 2))))
              (mt)
              (list (fdC 'quadruple 'x (appC 'double (appC 'double (idC 'x))))
                    (fdC 'double 'x (plusC (idC 'x) (idC 'x)))))
      22)
(interp (appC 'f1 (numC 3))
                  (mt)
                  (list (fdC 'f1 'x (appC 'f2 (numC 4)))
                        (fdC 'f2 'y (plusC (idC 'x) (idC 'y)))))
