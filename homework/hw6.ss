#lang racket
; problem 1.1
(cons 'a (cons '(b c)(cons 'd '(((e f) (g))))
))

;problem 1.2
(cons * (cons 'a (cons '4 '())))
((car(cons * (cons 'a '4))) 5 3)

;problem 2.1
(define flatten
  (lambda(l)
    (cond
      ((null? l) '())
      ((pair? l) (append (flatten (car l)) (flatten (cdr l))))
      (else (list l)))))

;problem 2.2
(define rev
  (lambda(l)
    (if (null? l) '()
        (if (pair? (car l))
            (append (rev (cdr l)) (cons (rev (car l)) '()))
            (append (rev (cdr l)) (cons (car l) '()))))))

;problem 2.3
(define delete
  (lambda (a l)
    (if (null? l) '()
    (if (pair? (car l)) (cons (delete a (car l)) (delete a (cdr l)))
    (if (eq? a (car l))
        (delete a (cdr l))
        (cons (car l) (delete a (cdr l))))))))

;problem 2.4
(define merge-sorted
  (lambda (x y)
    (if (null? x) y
    (if (null? y) x
    (if (eq? (car x) (car y))
        (merge-sorted (cdr x) y)
    (if (< (car x) (car y))
        (cons (car x) (merge-sorted (cdr x) y))
        (cons (car y) (merge-sorted (cdr y) x))))))))

;problem 3.1
(define NewTable
  (lambda ()
    '()))

;problem 3.2
(define InsertIntoTable
  (lambda (entry table)
    (cons entry table)))

;problem 3.3
(define LookupTable
  (lambda (variable table)
    (if (null? table) '()
    (if (eq? variable(car (car table)))
       (cadar table)
       (LookupTable variable (cdr table))))))

(define table
  (InsertIntoTable '(b (2 4 5)) (InsertIntoTable '(a 7) (NewTable))))


;problem 4
(define map
  (lambda (f l)
    (if (null? l)
        '()
        (cons (f (car l)) (map f (cdr l))))))

(define reduce
  (lambda (op l id)
    (if (null? l)
        id
        (op (car l) (reduce op (cdr l) id )))))

(define minSquareVal
  (lambda (l)
    (let ((square (map (lambda (i) (* i i)) l)) (x (* (car l ) (car l ))))
      (reduce (lambda (i j) (if (< i j) i j )) square x ))))

(define maxSquareVal
  (lambda (l)
    (let ((square (map (lambda (i) (* i i)) l)) (x (* (car l) (car l ))))
      (reduce (lambda (i j) (if (> i j) i j)) square x ))))
