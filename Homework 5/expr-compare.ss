#lang racket

(define (single-term-compare x y)
  (cond
    [(equal? x y) x]
    [(and (boolean? x) (boolean? y))
     (if x '% '(not %))]
    [else `(if % ,x ,y)]))

(define (expr-compare x y)
  (cond
    [(or (empty? x) (empty? y)) null]
    [else
     (cons (single-term-compare (car x) (car y)) (expr-compare (cdr x) (cdr y)))]))
     

          
    
