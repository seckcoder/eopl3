(module
  e2
  (lib "eopl.ss" "eopl")

  (require "utils.scm")

  ; 2.21
  ; Implement the data type of environments,
  ; as in section 2.2.2, using define-datatype.
  ; Then include has-binding? of exercise 2.9.
  (define-datatype
    env env?
    (empty-env)
    (non-empty-env
      (var symbol?)
      (value any-thing?)
      (base-env env?)))

  (define value?
    (lambda (v)
      #t))

  (define has-binding?
    (lambda (e search-var)
      (cases
        env e
        (empty-env () #f)
        (non-empty-env (var val base-env)
                       (or
                         (eqv? search-var var)
                         (has-binding? base-env search-var))))))

  ; 2.22

  ; stack ::= (empty-stack) | (val empty-stack)
  (define-datatype
    stack stack?
    (empty-stack)
    (non-empty-stack
      (val any-thing?)
      (stack stack?)))

  (define empty-stack?
    (lambda (st)
      (cases
        stack st
        (empty-stack () #t)
        (else #f))))

  (define push
    (lambda (val st)
      (non-empty-stack val st)))

  (define pop
    (lambda (st)
      (cases
        stack st
        (empty-stack () (eopl:error 'pop "empty stack"))
        (non-empty-stack (val rest-st) rest-st))))

  (define top
    (lambda (st)
      (cases
        stack st
        (empty-stack () (eopl:error 'top "empty stack"))
        (non-empty-stack (val rest-st) val))))

  (equal??
    (has-binding? (non-empty-env 'x 3 (empty-env)) 'x)
    #t)
  (equal??
    (has-binding? (non-empty-env 'x 4 (empty-env)) 'y)
    #f)
  (equal??
    (has-binding? (empty-env) 'x)
    #f)
  (report-unit-tests-completed 'has-binding?)
  (equal??
    (top (push 3 (push 5 (push 2 (empty-stack)))))
    3)
  (equal??
    (top (pop (push 3 (push 5 (push 2 (empty-stack))))))
    5)
  (equal??
    (empty-stack? (empty-stack))
    #t)
  (report-unit-tests-completed 'stack)
  )
