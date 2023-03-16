#lang racket
(define lst '(1 2 3 4 5 6 7 8 9 10))
(define redux-alt
  (lambda(fn lst)
    (cond
      [(empty? lst) '()]
      [(fn (car lst))(cons(car lst)
                          (redux-alt fn(cdr lst)))]
      [else(redux-alt fn(cdr lst))])))
(define add1
  (lambda(lst)
    (cond
      [(empty? lst) '()]
      [else(cons(+ (car lst) 1)
                (add1(cdr lst)))])))
(define squares
  (lambda(lst)
    (cond
      [(empty? lst) '()]
      [else(cons(*(car lst)(car lst))
                (squares(cdr lst)))])))
(define map-alt
  (lambda(fn lst)
    (cond
      [(empty? lst)'()]
      [else(cons(fn(car lst))
                (map-alt fn(cdr lst)))])))
(define deep-search-list
    (lambda (n lst)
        (cond
            [(empty? lst) #f]
            [(list? (car lst)) (or (deep-search-list n (car lst))
                                   (deep-search-list n (cdr lst)))]
            [(eq? n (car lst)) #t]
            [else (deep-search-list n (cdr lst))]
        )
    )
)

;;; Deep list
(define deep-list '((1 2 (3) (4 5 (6)) 7 (8 (9 10)))))

;;; Deep list (recursive) : a list of random numbers
;;; (deep-list-rec 3) => ((1 2 (3) (4 5 (6)) 7 (8 (9 10))))
(define deep-list-rec
    (lambda (n)
        (cond
            [(= n 0) '()]
            [else (cons (random 10) (deep-list-rec (- n 1)))]
        )
    )
)

;;; Deep sum list (recursive) : sum of all elements in a deep list 
;;; (deep-list 3) => 55
(define deep-sum-list
    (lambda (deep-lst)
        (cond
            [(empty? deep-lst) 0]
            [(list? (car deep-lst)) (+ (deep-sum-list (car deep-lst))
                                       (deep-sum-list (cdr deep-lst)))]
            [else (+ (car deep-lst) (deep-sum-list (cdr deep-lst)))]
        )
    )
)


(define reverse
  (lambda(lst)
    (cond
      [(empty? lst) '()]
      [else(append(reverse(cdr lst))
                  (cons(car lst)'()))])))