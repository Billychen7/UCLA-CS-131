#lang racket

(define (single-term-compare x y)
  (cond
    [(equal? x y) x]
    [(and (boolean? x) (boolean? y))
     (if x '% '(not %))]
    [else `(if % ,x ,y)]))


(define (expr-compare x y)
  (cond
    [(or (empty? x) (empty? y)) empty] ; for now just return empty list if one of the lists is empty (currently assuming that both lists are of the same length)
    ; insert the cases for not being a pair here (expr-compare 1 2)
    [else
     (let ([x-head (car x)] [y-head (car y)] [x-tail (cdr x)] [y-tail (cdr y)])
       (cond
         [(and (pair? x-head) (pair? y-head)) ;if the heads are pairs
          (cons (expr-compare x-head y-head) (expr-compare x-tail y-tail))]
         [else ;if the heads are just normal atoms
          (if (and (equal? x-head 'quote) (equal? y-head 'quote)) ; if the head is a quote, then just treat as data
              (cons (single-term-compare (cons 'quote (car x-tail)) (cons 'quote (car y-tail))) (expr-compare (cdr x-tail) (cdr y-tail)))
              (cons (single-term-compare x-head y-head) (expr-compare x-tail y-tail)))]))]))
              
              

     

          
    
