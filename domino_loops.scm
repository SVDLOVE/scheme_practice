;(define domino-loops
;    (lambda (n)
;        (filter loop? (permutations (dominoes n)))
;    )
;)
;
;(define filter
;    (lambda (f L)
;    ; return list of those elements in L which pass through filter f
;        (if (null? L)
;            L
;            (let ((N (f (car L))))
;                (if (null? N)
;                    (filter f (cdr L))
;                    (cons N (filter f (cdr L)))
;                )
;            )
;        )
;    )
;)

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

;(define dominoes 
;    (lambda (n)
;        (let (
;                (ll '(0 1 2 3 4 5))
;                (lr (build-my-inc-list  0 n))
;            )
;            (map cons ll lr)
;        )
;    )
;)



;(define (permut ls)
;  (if (null? ls)
;      '(())
;      (apply append
;             (map (lambda (l) (insert-perm (car ls) l))
;                  (permut (cdr ls))))))
;
;(define (insert-perm x ls)
;  (if (null? ls)
;      (list (list x))
;      (cons (cons x ls)
;            (map (lambda (l) (cons (car ls) l))
;                 (insert-perm x (cdr ls))))))