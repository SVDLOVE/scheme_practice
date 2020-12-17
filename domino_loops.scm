(define domino-loops
    (lambda (n)
        (filter loop? (permutations (dominoes n)))
    )
)

(define filter
    (lambda (f L)
    ; return list of those elements in L which pass through filter f
        (if (null? L)
            L
            (let ((N (f (car L))))
                (if (null? N)
                    (filter f (cdr L))
                    (cons N (filter f (cdr L)))
                )
            )
        )
    )
)

(define dominoes 
    (lambda (n)
        (let (
                (ll (build-my-same-list 0 n))
                (lr (build-my-inc-list  0 n))
            )
            (map cons ll lr)
        )
    )
)

(define (permutations ls)
    (if (null? ls)
        '(())
        (apply append
             (map 
                (lambda (l) 
                    (insert-perm (car ls) l)
                )
                (permutations (cdr ls))
            )
        )
    )
)

(define loop? 
    (lambda (L)
        (let*
            (
                (fl (cond 
                        ((= (cdar L) (caadr L) ) (car L))
                        ((= (caar L) (caadr L) ) (cons (cdar L) (caar L)))
                        ((= (caar L) (cdadr L) ) (cons (cdar L) (caar L)))
                        (else (car L))
                    )
                )
                (ll (compare_list_is_loop fl fl (cdr L)) )
            )
            (if(< (length ll) (length L)) '()
                ll
            )
        )
    )
)



(define (compare_list_is_loop fl cl lsl)
    (cond 
        ((null? lsl) 
            (if(equal? (car fl)  (cdr cl)) (list fl)
                '()
            )
        )
        (else
            (cond
                ((equal? (cdr cl) (caar lsl))  
                    (cons 
                        (car lsl) 
                        (compare_list_is_loop fl (car lsl) (cdr lsl)) 
                    ) 
                )
                ((equal?  (cdr cl) (cdar lsl))  
                    (cons 
                        (cons (cdar lsl) (caar lsl)) 
                        (compare_list_is_loop fl (cons (cdar lsl) (caar lsl)) (cdr lsl))  
                    ) 
                )
                (else '())
            )
        )
    )
)

(define (insert-perm x ls)
    (if (null? ls)
        (list (list x))
        (cons (cons x ls)
            (map 
                (lambda (l) 
                    (cons (car ls) l)
                )
                (insert-perm x (cdr ls))
            )
        )
    )
)

(define build-same-list 
    (lambda (n1 n2)
        (if(<=  n2 0) '()
            (append (list n1) (build-same-list n1 (- n2 1)))
        )
    )
)

(define build-inc-list 
    (lambda (n1 n2)
        (if(> n1 n2 ) '()
            (append (list n1) (build-inc-list (+ n1 1) n2))
        )
    )
)

(define build-my-inc-list 
    (lambda (n1 n2)
        (if(> n1 n2 ) '()
            (append (build-inc-list n1 n2) (build-my-inc-list (+ n1 1) n2))
        )
    )
)

(define build-my-same-list 
    (lambda (n1 n2)
        (if(> n1 n2 ) '()
            (append (build-same-list n1 (+ (- n2 n1) 1)) (build-my-same-list (+ n1 1) n2))
        )
    )
)