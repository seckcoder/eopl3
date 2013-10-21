#lang eopl

(define identifier?
  (lambda (sym)
    (and (symbol? sym)
         (not (eq? sym 'lambda)))))

(define-datatype lc-exp lc-exp? 
                 (var-exp
                   (var identifier?))
                 (lambda-exp
                   (bound-var identifier?)
                   (body lc-exp?))
                 (app-exp
                   (rator lc-exp?)
                   (rand lc-exp?)))
(define parse-expression
  (lambda (datum)
    (cond
      ((symbol? datum) (var-exp datum))
      ((pair? datum)
       (if (eq? (car datum) 'lambda)
         (lambda-exp
           (car (cadr datum))
           (parse-expression (caddr datum)))
         (app-exp
           (parse-expression (car datum))
           (parse-expression (cadr datum)))))
      (else
        (eopl:error 'parse-expression "Invalid syntax:~s" datum)))))


(define unparse-lc-exp
  (lambda (exp)
    (cases
      lc-exp exp
      (var-exp (var) var)
      (lambda-exp (bound-var body)
                  (list 'lambda (list bound-var) (unparse-lc-exp body)))
      (app-exp (rator rand)
               (list (unparse-lc-exp rator) (unparse-lc-exp rand))))))


(let ((exps (call-with-input-file
              "test-read-file.scm"
              (lambda (in)
                (let loop ((datum (read in)))
                  (cond ((eof-object? datum)
                         '())
                        (else
                          (cons (parse-expression datum)
                                (loop (read in))))))))))
  (display exps))
