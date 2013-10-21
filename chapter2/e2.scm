#lang eopl

(require racket
         "utils.scm")

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

; 2.23
; trivial. just define a func that only accept symbol other than lambda.


; 2.24
(define-datatype
  bintree bintree?
  (leaf-node
    (num integer?))
  (interior-node
    (key symbol?)
    (left bintree?)
    (right bintree?)))

(define bintree-to-list
  (lambda (bt)
    (cases
      bintree bt
      (leaf-node (num) (list 'leaf-node num))
      (interior-node (key left right) (list 'interior-node
                                            key
                                            (bintree-to-list left)
                                            (bintree-to-list right))))))

; 2.25

(define bintree-sum->num
  (lambda (bt-sum)
    (cond ((eq? (car bt-sum)
                'leaf-node)
           (cadr bt-sum))
          ((interior-node-sum? bt-sum)
           (caddr bt-sum)))))

(define interior-node-sum?
  (lambda (bt-sum)
    (eq? (car bt-sum)
         'interior-node)))

(define interior-node-sum-symbol
  (lambda (bt-sum)
    (cadr bt-sum)))

(define make-leaf-node-sum
  (lambda (num)
    (list 'leaf-node num)))

(define make-interior-node-sum
  (lambda (sym num)
    (list 'interior-node sym num)))

(define bintree-sums
  (lambda (bt)
    (cases
      bintree bt
      (leaf-node (num) (list (make-leaf-node-sum num)))
      (interior-node (key left right)
                     (let* ((left-bintree-sums (bintree-sums left))
                            (right-bintree-sums (bintree-sums right))
                            (left-root-sum (car left-bintree-sums))
                            (right-root-sum (car right-bintree-sums)))
                       (cons (make-interior-node-sum key
                                                     (+ (bintree-sum->num left-root-sum)
                                                        (bintree-sum->num right-root-sum)))
                             (append left-bintree-sums
                                     right-bintree-sums)))))))
(define max-interior
  (lambda (bt)
    (interior-node-sum-symbol (mmax (lambda (bt-sum1 bt-sum2)
            (> (bintree-sum->num bt-sum1)
               (bintree-sum->num bt-sum2)))
          (filter (lambda (bt-sum)
                    (interior-node-sum? bt-sum))
                  (bintree-sums bt))))))

(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
          (and (pair? val)
               (pred (car val))
               ((list-of pred) (cdr val)))))))

(define-datatype
  red-blue-tree red-blue-tree?
  (red-node
    (rbtree-left red-blue-tree?)
    (rbtree-right red-blue-tree?))
  (blue-node
    (rbtree red-blue-tree?)
    (rbtrees (list-of red-blue-tree?)))
  (rbtree-leaf-node
    (Int integer?)))

(define rbtree-transform
  (lambda (rbtree)
    (let loop ((rbtree rbtree)
               (red-node-num 0))
    (cases
      red-blue-tree rbtree
      (rbtree-leaf-node (Int)
                 (rbtree-leaf-node red-node-num))
      (red-node (rbtree-left rbtree-right)
                (red-node
                  (loop rbtree-left
                        (+ red-node-num 1))
                  (loop rbtree-right
                        (+ red-node-num 1))))
      (blue-node (rbtree rbtrees)
                 (blue-node (loop rbtree
                                  red-node-num)
                            (map (lambda (rbtree)
                                   (loop rbtree red-node-num))
                                 rbtrees)))))))


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
(equal??
  (bintree-to-list (interior-node 'a (leaf-node 3) (leaf-node 4)))
  '(interior-node a (leaf-node 3) (leaf-node 4)))
(report-unit-tests-completed 'bintree-to-list)

(define tree-1
  (interior-node 'foo (leaf-node 2) (leaf-node 3)))
(define tree-2
  (interior-node 'bar (leaf-node -1) tree-1))
(define tree-3
  (interior-node 'baz tree-2 (leaf-node 1)))

(equal??
  (max-interior tree-3)
  'baz)
(equal??
  (max-interior tree-2)
  'foo)

(report-unit-tests-completed 'max-interior)

(define rbtree1
  (rbtree-leaf-node 3))

(define rbtree2
  (rbtree-leaf-node 2))

(define rbtree3
  (blue-node rbtree1 (list rbtree2)))

(define rbtree4
  (red-node rbtree1 rbtree2))

(define rbtree5
  (red-node rbtree3 rbtree4))

(define rbtree6
  (blue-node rbtree5 '()))

(define rbtree7
  (red-node rbtree6 rbtree5))

(rbtree-transform rbtree7)

