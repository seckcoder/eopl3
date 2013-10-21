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

  (equal??
    (has-binding? (non-empty-env 'x 3 (empty-env)) 'x)
    #t)
  (equal??
    (has-binding? (non-empty-env 'x 4 (empty-env)) 'y)
    #f)
  (equal??
    (has-binding? (empty-env) 'x)
    #f)
  )
