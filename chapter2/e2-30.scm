#lang eopl
; lc-exp ::= symbol
; lc-exp ::= (lambda (symbol) lc-exp)
; lc-exp ::= (lc-exp lc-exp)

(define identifier?
  (lambda (sym)
    (and (symbol? sym)
         (not (eq? sym 'lambda)))))
(define-datatype
  lc-exp lc-exp? 
  (var-exp
    (var identifier?))
  (lambda-exp
    (bound-var identifier?)
    (body lc-exp?))
  (app-exp
    (rator lc-exp?)
    (rand lc-exp?)))

; a more robust implemention to parse lambda exp
; apparently, pattern-matching is better to used here.
(define parse-expression
  (lambda (datum)
    (cond
      ((symbol? datum) (var-exp datum))
      ((pair? datum)
       (if (eq? (car datum) 'lambda)
         (let ((var-body-exp (cdr datum)))
           (cond ((= (length var-body-exp)
                     2)
                  (lambda-exp
                    (car (cadr datum))
                    (parse-expression (caddr datum))))
                 (else
                   (eopl:error 'parse-expression "format:`(lambda (sym) lc-exp)` is not correct: ~s" datum))))
         (begin
           (if (= (length datum) 2)
             (app-exp
               (parse-expression (car datum))
               (parse-expression (cadr datum)))
             (eopl:error 'parse-expression "format: `(lc-exp lc-exp) is not correct: ~s" datum)))))
      (else
        (eopl:error 'parse-expression "Invalid syntax:~s" datum)))))

(let ((exps (call-with-input-file
              "test-e2-30.scm"
              (lambda (in)
                (let loop ((datum (read in)))
                  (cond ((eof-object? datum)
                         '())
                        (else
                          (cons (parse-expression datum)
                                (loop (read in))))))))))
  (display exps))
