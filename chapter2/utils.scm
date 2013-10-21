#lang eopl

(require racket)
;; a very simple macro for inline testing

(provide equal?? report-unit-tests-completed any-thing? mmax)

;; simple-minded magic for tests
(define-syntax equal??
  (syntax-rules ()
                ((_ x y)
                 (let ((x^ x) (y^ y))
                   (if (not (equal? x y))
                     (eopl:error 'equal??
                                 "~s is not equal to ~s" 'x 'y)
                     #t)))))

(define report-unit-tests-completed
  (lambda (fn-name)
    (eopl:printf "unit tests completed: ~s~%" fn-name)))

(define any-thing?
  (lambda (v)
    #t))

(define mmax
  (lambda (proc lst)
    (if (null? lst)
      (eopl:error 'mmax
                  "empty list")
      (foldr (lambda (accum v)
               (if (proc accum v)
                 accum
                 v))
             (car lst)
             lst))))
