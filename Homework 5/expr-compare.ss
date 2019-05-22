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

(define dick #hash((a . "apple") (b . "banana")))

; we'll see what the best behavior is for when they match - for now ill not add anything to the dict - consider doing the op twice

(define (populate-var-dict x-var-names y-var-names x-var-dict y-var-dict)
  (if (and (empty? x-var-names) (empty? y-var-names))
      (list x-var-dict y-var-dict) ; if we've gone through all variables, return the dicts
      (let ([x-var (car x-var-names)] [y-var (car y-var-names)] [x-tail (cdr x-var-names)] [y-tail (cdr y-var-names)])
        (if (equal? x-var y-var)
            (populate-var-dict x-tail y-tail x-var-dict y-var-dict) ; if they're equal, don't add entries to the dictionaries
            (populate-var-dict x-tail y-tail (dict-set x-var-dict x-var y-var)(dict-set y-var-dict y-var x-var))))))

(define (lambda-compare x y)
  (let ([lambda-form
         (if (not (equal? (car x) (car y))) ; if at least one phrase used the symbol version, then use that for both versions
             'λ
             (car x))]
        [x-var-names (cadr x)]
        [y-var-names (cadr y)]
        [var-dicts (populate-var-dict (cadr x) (cadr y) '#hash() '#hash())])
        ;[x-var-dict (car var-dicts)]
        ;[y-var-dict (cadr var-dicts)])
    (cons lambda-form (lambda-body-compare (cdr x) (cdr y) (car var-dicts) (cadr var-dicts)))))


; (lam
(define (lambda-body-compare x y x-var-dict y-var-dict)
  (cons 1 (2 3)))

; this is horrificly inefficient but will optimize later
; (lambda-single-term-compare 'a 'b '#hash((a . b)) '#hash((b . a)))
(define (lambda-single-term-compare x y x-var-dict y-var-dict)
  (cond
    [(equal? x y) x]
    [(and (boolean? x) (boolean? y)) ; probably don't have to check for this
     (if x '% '(not %))]
    [else
     (let ([x-mapped (dict-ref x-var-dict x #f)] [y-mapped (dict-ref y-var-dict y #f)])
       (if (and (equal? x-mapped y) (equal? y-mapped x))
           (string->symbol (string-append (symbol->string x) "!" (symbol->string y)))
           `(if % ,x ,y)))]))



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

     

          
    
