(define (curry-cook formals body) 
  (if (= (length formals) 1)
    `(lambda ,formals ,body)
    `(lambda (,(car formals)) ,(curry-cook (cdr formals) body))))

(define (curry-consume curry args)
  (if (null? args)
    curry
    (curry-consume (curry (car args)) (cdr args))))

(define-macro (switch expr options)
  (switch-to-cond (list 'switch expr options)))

(define (switch-to-cond switch-expr)
  (cons 'cond
        (map (lambda (option)
               (cons (cons 'equal? (cons (car (cdr switch-expr)) (cons (car option) nil))) (cdr option)))
             (car (cdr (cdr switch-expr))))))