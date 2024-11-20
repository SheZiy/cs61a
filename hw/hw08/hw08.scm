(define (ascending? s) 
    (if (<= (length s) 1)
        #t
        (if (> (car s) (car (cdr s)))
            #f
            (ascending? (cdr s)))))

(define (my-filter pred s) 
    (if (null? s)
        s
        (if (not (pred (car s)))
            (my-filter pred (cdr s))
            (cons (car s) (my-filter pred (cdr s))))))

(define (interleave lst1 lst2) 
    (cond 
        ((null? lst1) lst2)
        ((null? lst2) lst1)
        (else (cons (car lst1) (interleave lst2 (cdr lst1))))))

(define (no-repeats s) 
    (if (null? s)
        s
        (let 
            ((filtered-s (filter (lambda (x) (not (= (car s) x))) (cdr s))))
            (cons (car s) (no-repeats filtered-s))
        )
    )
)