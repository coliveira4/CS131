#lang racket
;global lambda variable definition, from TA
(define LAMBDA (string->symbol "\u03BB"))

(define (convert_to_list x) ;to convert non-lists to lists
  (cond 
    [(list? x) x] 
    [else (list x)]))

(define (is_any_lambda? symbol) 
  (cond
    ;check if equal to literal lambda or equal to global lambda symbol
    [ (or (equal? symbol 'lambda) (equal? symbol LAMBDA))
      #t
    ]
    [ else
      #f
    ]))

(define (is_lambda_expr? symbol) ;if all true then return true, as symbol is a lambda expressiojn
  (cond
    [ (and 
        (list? symbol) ;is it a list
        (is_any_lambda? (car symbol)) ;does it start with any kind of lambda symbol
        (equal? (length symbol) 3)) ;is length of given 3
        #t
    ]
    [ else
      #f
    ]))

(define (is_lambda_expr_wrapper symb1 symb2)
  (cond
    [(and 
      (is_lambda_expr? symb1) 
      (is_lambda_expr? symb2)) ;are they both lambda expressions
      (lambda_case symb1 symb2 (cadr symb1) (cadr symb2))] ;send to lambda case
    ))

(define (insert_excl symb1 symb2) ;inserts !
  (string->symbol 
    (string-append (symbol->string symb1) "!" (symbol->string symb2))))

(define (add_dict_entry parameters1 parameters2 d1 d2)
  (cond
    [(and 
      (empty? parameters1) 
      (empty? parameters2)) 
      (list d1 d2)] ;exists in dict then dont need to add
    [(equal? (car parameters1) (car parameters2)) 
      (add_dict_entry (cdr parameters1) (cdr parameters2) d1 d2)]
    [else
      (let (
        (d1_new (dict-set d1 (car parameters1) (insert_excl (car parameters1) (car parameters2))))
        (d2_new (dict-set d2 (car parameters2) (insert_excl (car parameters1) (car parameters2)))))
        (add_dict_entry (cdr parameters1) (cdr parameters2) d1_new d2_new))]
      )
  )

(define (add_dict_entry_wrapper p1 p2)
  (add_dict_entry (convert_to_list p1) (convert_to_list p2) #hash() #hash())
  )

(define (base_case x symb) 
  (dict-ref x symb symb)
  )

(define (add_entries x entries)
  (cond
    [(or (equal? (dict-count entries) 0) (empty? x)) ;no entries or expr empty, return expr
      x]

    [(and (list? (car x)) (is_lambda_expr? (car x))) ;else if lambda expression 
      (let ((new_lambda (car x)))
      (let ((fix_entries (erase_entries entries (convert_to_list (cadr new_lambda)))))
      (let ((lambda_fix (append (list (car new_lambda) (cadr new_lambda)) (add_entries (cddr new_lambda) fix_entries))))
      (cons lambda_fix (add_entries (cdr x) entries)))))]

    [(list? (car x)) (cons (add_entries (car x) entries) (add_entries (cdr x) entries))]

    [else (cons (base_case entries (car x)) (add_entries (cdr x) entries))]))

(define (erase_entries dic to_erase)
  (cond
    [(or (equal? (dict-count dic) 0) (empty? to_erase)) 
      dic] ; if key contains nothing return dic
    [else 
      (let ((new_dict (dict-remove dic (car to_erase))))
      (erase_entries new_dict (cdr to_erase)))]
  )
); recurse on list of keys to erase all keys in to erase

(define (compare_lambdas s1 s2)
  (cond
    [(and (empty? s1) (empty? s2)) '()] ;base case
    [else (expr-compare s1 s2)] ;recurse
    )
  ) 

(define (lambda_case s1 s2 s1_arg s2_arg)
  (cond
    [(let ((dict_s1 (car (add_dict_entry_wrapper s1_arg s2_arg))) (dict_s2 (cadr (add_dict_entry_wrapper s1_arg s2_arg))))
      (let ((s1_entry (cons (car s1) (add_entries (cdr s1) dict_s1))) 
            (s2_entry (cons (car s2) (add_entries (cdr s2) dict_s2))))
      (append (list LAMBDA (cadr s1_entry))
      (compare_lambdas (cddr s1_entry) (cddr s2_entry)))))]
   )
  )

(define (next_layer s1 s2)
  (cond
    [(or (empty? s1) (empty? s2)) '()] ;base case, are either empty?
    [else (cons (expr-compare (car s1) (car s2)) ;recurse
      (next_layer (cdr s1) (cdr s2)))]))

(define (equal_start s1 s2 function)
  (cond
    [(equal? function 'quote) 
      (non_equal_lengths s1 s2)] ;quote case?
    [(is_any_lambda? function) 
      (is_lambda_expr_wrapper s1 s2)] ;lambda case?
    [else (next_layer s1 s2)])) ;else recurse

(define (unequal_start s1 s2) 
  (cond
    [(or (equal? (car s1) 'quote) (equal? (car s2) 'quote);quote case
      (equal? (car s1) 'if) (equal? (car s2) 'if))
      (non_equal_lengths s1 s2)] 
    [(is_any_lambda? (car s1)) ;lambda case for s1?
      (non_equal_lengths (cons LAMBDA (cdr s1)) s2)]
    [(is_any_lambda? (car s2))  ;s2?
      (non_equal_lengths s1 (cons LAMBDA (cdr s2)))]      
    [else (next_layer s1 s2)]
    )
  )

(define (non_equal_lengths s1 s2)
  (cond
    [(equal? s1 s2)
      s1] ;if equal just return s1
    [(and (equal? s1 #f) (equal? s2 #t)) ;the boolean cases
      '(not %)]
    [(and (equal? s1 #t) (equal? s2 #f))
      '%]
    [else (list 'if '% s1 s2)])) ;if % then return x otherwise y

;expr_compare core defintion
(define (expr-compare x y)
  (cond
    [(or (not (and (list? x) (list? y))) (not (equal? (length x) (length y)))) ;not both lists? or not equal lengths?
    (non_equal_lengths x y)] 
    [(or (equal? (car x) (car y)) (and (is_any_lambda? (car x)) (is_any_lambda? (car y)))) ;first symbol equal or are both any kind of lambda
    (equal_start x y (car x))]
    [else (unequal_start x y)])) ;otherwise



;test case
(define (test-expr-compare x y)
  (and (equal? (eval x) (eval (list 'let '((% #t)) (expr-compare x y)))) (equal? (eval y) (eval (list 'let '((% #t)) (expr-compare x y)))))
)

(define test-expr-x
  (list
    (+ 1 2)
    (quote (1 2))
    (lambda (x) (- x x) 10)
    (if #t 1 2)
  )
)

(define test-expr-y
  (list
    (* 1 2)
    (quote (* 1 2))
    (lambda (x y) (+ x y) 10)
    (if #f 1 2)
    ''(1 2)
  )
)

;thee are confirmed working now
;these should return the symbol lambda
;(expr-compare 'λ 'lambda)
;(expr-compare 'lambda 'lambda)
#|
(expr-compare 12 12)
(expr-compare 12 20)
(expr-compare #t #t)
(expr-compare #f #f)
(expr-compare #t #f)
(expr-compare #f #t)
(expr-compare 'a '(cons a b))
(expr-compare '(cons a b) '(cons a b))

;uhoh
(expr-compare '(cons a lambda) '(cons a λ))

(expr-compare '(cons (cons a b) (cons b c))
              '(cons (cons a c) (cons a c)))
(expr-compare '(cons a b) '(list a b))
(expr-compare '(list) '(list a))
(expr-compare ''(a b) ''(a c))
(expr-compare '(quote (a b)) '(quote (a c)))
(expr-compare '(quoth (a b)) '(quoth (a c)))
(expr-compare '(if x y z) '(if x z z))
(expr-compare '(if x y z) '(g x y z))
(expr-compare '((lambda (a) (f a)) 1) '((lambda (a) (g a)) 2))
(expr-compare '((lambda (a) (f a)) 1) '((λ (a) (g a)) 2))
(expr-compare '((lambda (a) a) c) '((lambda (b) b) d))
(expr-compare ''((λ (a) a) c) ''((lambda (b) b) d))
(expr-compare '(+ #f ((λ (a b) (f a b)) 1 2))
              '(+ #t ((lambda (a c) (f a c)) 1 2)))
(expr-compare '((λ (a b) (f a b)) 1 2)
              '((λ (a b) (f b a)) 1 2))
(expr-compare '((λ (a b) (f a b)) 1 2)
              '((λ (a c) (f c a)) 1 2))
(expr-compare '((lambda (lambda) (+ lambda if (f lambda))) 3)
              '((lambda (if) (+ if if (f λ))) 3))
(expr-compare '((lambda (a) (eq? a ((λ (a b) ((λ (a b) (a b)) b a))
                                    a (lambda (a) a))))
                (lambda (b a) (b a)))
              '((λ (a) (eqv? a ((lambda (b a) ((lambda (a b) (a b)) b a))
                                a (λ (b) a))))
                (lambda (a b) (a b))))
                |#

