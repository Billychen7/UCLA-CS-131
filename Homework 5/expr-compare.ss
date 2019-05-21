#lang racket

(define (single-term-compare x y)
  (cond
    [(equal? x y) x]
    [(and (boolean? x) (boolean? y))
     (if x '% '(not %))]
    [else `(if % ,x ,y)]))

(define (quote-compare x y)
  (if (equal? x y)
  x
  `(if % ,x ,y)))

(define (expr-compare x y)
  (cond
    [(or (empty? x) (empty? y)) empty] ; for now just return empty list if one of the lists is empty (currently assuming that both lists are of the same length)
    [(or (not (pair? x)) (not (pair? y)))
     (single-term-compare x y)]
     ; if both
    ; add cases for one pair, etc
    [else
     (let ([x-head (car x)] [y-head (car y)] [x-tail (cdr x)] [y-tail (cdr y)])
       (cond
         [(and (pair? x-head) (pair? y-head)) ;if the heads are pairs
          (cons (expr-compare x-head y-head) (expr-compare x-tail y-tail))]
         [else ;if the heads are just normal atoms
          (if (and (equal? x-head 'quote) (equal? y-head 'quote)) ; if the head is a quote, then just treat as data
              (append (quote-compare `',(car x-tail) `',(car y-tail)) (expr-compare (cdr x-tail) (cdr y-tail)))
              (cons (single-term-compare x-head y-head) (expr-compare x-tail y-tail)))]))]))
              
              

     

          
    
