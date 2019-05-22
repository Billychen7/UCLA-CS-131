#lang racket

; handle (expr-compare '(a) '(a b)) -> should be '(a b) I think
; fix list vs list a

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

(define (lambda-compare x y)
  (let ([lambda-form
         (if (not (equal? (car x) (car y))) ; if at least one phrase used the symbol version, then use that for both versions
             'λ
             (car x))])
    (cons lambda-form (expr-compare (cdr x) (cdr y)))))

(define (expr-compare x y)
  (cond
    [(or (empty? x) (empty? y)) empty] ; for now just return empty list if one of the lists is empty (currently assuming that both lists are of the same length)
    [(or (not (pair? x)) (not (pair? y)))
     (single-term-compare x y)]
    [(xor (equal? (length x) 1) (equal? (length y) 1)) ; this is for the list vs list a case - weird
     (single-term-compare x y)]
     ; if both
    ; add cases for one pair, etc
    [else
     (let ([x-head (car x)] [y-head (car y)] [x-tail (cdr x)] [y-tail (cdr y)])
       (cond
         [(and (pair? x-head) (pair? y-head)) ;if the heads are pairs
          (cond
            [(and (or (equal? (car x-head) 'lambda) (equal? (car x-head) 'λ)) (or (equal? (car y-head) 'lambda) (equal? (car y-head) 'λ)))
             (cons (lambda-compare x-head y-head) (expr-compare x-tail y-tail))]
            [else (cons (expr-compare x-head y-head) (expr-compare x-tail y-tail))])]
         [else ;if the heads are just normal atoms
          (cond
            [(and (equal? x-head 'quote) (equal? y-head 'quote)) ; if the head is a quote, then just treat as data
             (append (quote-compare `',(car x-tail) `',(car y-tail)) (expr-compare (cdr x-tail) (cdr y-tail)))]
            [(xor (equal? x-head 'if) (equal? y-head 'if)) ; if only one of the heads is an 'if'
             (single-term-compare x y)] ; this is probably wrong
            [else (cons (single-term-compare x-head y-head) (expr-compare x-tail y-tail))])]))]))

     

          
    
