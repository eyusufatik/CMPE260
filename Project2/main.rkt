; esad yusuf atik
; 2020400261
; compiling: yes
; complete: yes

#lang racket

(provide (all-defined-out))

; 10 points
(define :=
  (lambda (var value)
    (list var value)
    )
  )

; 10 points
(define --
  (lambda args
    (if (> (length args) 1)
        (list 'let args)
        (cons 'let  args)
        )
    )
  )



; 10 points
(define @
  (lambda (bindings expr)
    (append bindings expr)
    )
  )


; 20 points
(define split_at_delim
  (lambda (delim args)
    ;(display args)
    (if (> (length (indexes-of args delim)) 0)
        ;(display (list (take args index)))
        (append (list (take args (car (indexes-of args delim))))
                (split_at_delim delim (list-tail args (+ (car  (indexes-of args delim)) 1) ))

                )
        (list args)
        )
    )
  )
  
; 30 points
(define recursive_parse
  (lambda (expr_list)
    ;(display expr_list)
    (if (> (length expr_list) 1)
        (let ( (first_expr (parse_expr (car expr_list))) (rest_expr (recursive_parse (cdr expr_list))))
          (if (list? first_expr)
              (if (or  (equal? (car first_expr) '+) (equal? (car first_expr) '*) (equal? (car first_expr) 'let))
                  (if (list? rest_expr)
                      (if (or  (equal? (car rest_expr) '+) (equal? (car rest_expr) '*) (equal? (car rest_expr) 'let))
                          (append (list first_expr) (list rest_expr))
                          (append (list first_expr) rest_expr)
                          )
                      (list  first_expr rest_expr)
                  )
                  (if (list? rest_expr)
                      (if (or  (equal? (car rest_expr) '+) (equal? (car rest_expr) '*) (equal? (car rest_expr) 'let))
                          (append  first_expr (list rest_expr))
                          (append first_expr rest_expr)
                          )
                      (list first_expr rest_expr)
                  )
              )
              (if (list? rest_expr)
                  (if (or  (equal? (car rest_expr) '+) (equal? (car rest_expr) '*) (equal? (car rest_expr) 'let))
                      (append (list first_expr) (list rest_expr))
                      (append (list first_expr) rest_expr)
                  )
                  (list first_expr rest_expr)
              )
             ) 
          )
          
        (parse_expr (car expr_list)) 
        )
    )
)
; (parse_expr '(1 + 7 + 6 + 1 + 34 * (('x := 3 -- 'y := 6 --) @ (5 * (x + y + ('z := 'x --) @ (z)))) + 5))

(define parse_plus_operation
  (lambda (expr)
    (append '(+)  (recursive_parse (split_at_delim '+ expr)))
    )
  )

(define parse_multiply_operation
  (lambda (expr)
     (append '(*) (recursive_parse (split_at_delim '* expr)))
    )
  )

(define parse_assignments
  (lambda (expr)
     (map (lambda (assignment)
         (:=   (eval (first assignment)) (eval (third assignment)))) expr)
    )
  )



(define parse_binding_list
  (lambda (expr)
    ;(display expr)
    (--  (parse_assignments (remove empty (split_at_delim '-- expr))))
    )
  )
;((('x := 5 --)) ((x + 1.618)))
(define parse_binding_operation
  (lambda (expr)
     (let ((expr_list (split_at_delim '@  expr)))
       ;(display (parse_binding_list (caar expr_list)))
       (@ (parse_binding_list (caar expr_list)) (list (parse_expr (cdr expr_list))))
       )
    )
  )


(define parse_expr
  (lambda (expr)
    (if (list? expr)
        (if (equal? (length expr) 1)
             (parse_expr (car expr))
        
            (cond [(not (equal? (split_at_delim '+ expr) (list expr))) 
                   (parse_plus_operation expr)]
                  [(not (equal? (split_at_delim '* expr) (list expr)))
                   (parse_multiply_operation expr)]
                  [(not (equal? (split_at_delim '@ expr) (list expr)))
                   (parse_binding_operation expr)]
                  ;(not (equal? (split_at_delim '@ expr) expr))
                  ;    empty
                  ;(else
                  ;    empty
                  ;  )
           
                  )
         
    
            )
         expr
        )
    )

)

  ; 20 points
(define eval_expr
    (lambda (expr)
      (eval (parse_expr expr))))
  