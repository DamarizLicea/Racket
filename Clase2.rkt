#lang racket
(define atendees
  (lambda (ticket-price)
    (+ 120
     (*(/ 15 0.10)(- 5.00 ticket-price)))))
;;atendees 4.90->134.90
(atendees 4.90)

(define revenue
  (lambda(ticket-price)
    (*(atendees ticket-price) ticket-price)))

(revenue 4.90)


(define cost
  (lambda(ticket-price)
    (+ 180
       (* 0.04(atendees ticket-price)))))

(cost 4.90)

(define best-ticket-price
  (lambda(ticket-price best)
    (cond
      [(<= ticket-price 0.0)'not_found]
      [(>(-(revenue ticket-price)
         (cost ticket-price))best)
      (best-ticket-price(- ticket-price 0.10)
                           (-(revenue ticket-price)
                             (cost ticket-price)))]
      [else ticket-price])))

(best-ticket-price 5.0 0)



;;añadir listas

(define lst '(1 2 3 4 5 6 7 8 9 10))
;;car=first
(car lst)
(first lst)
(cdr lst)
(rest lst)
(cadr lst)
(caddr lst)


;;funcion que me diga la longitud de la lista
;;null=empty

(define size
  (lambda(lst)
    (cond
      [(empty? lst)0]
      [else(+ 1(size(cdr lst)))])))
(size lst)

(define size-tail
  (lambda (lst acc)
    (cond
      [(empty? lst)acc]
      [else(size-tail(rest lst)(+ acc 1))])))

(define sum-list-head
  (lambda(lst)
    (cond
      [(empty? lst)0]
      [else (+ (car lst)(sum-list-head(cdr lst)))])))

(sum-list-head lst)

(define sum-list-tail
  (lambda(lst acc)
    (cond
      [(empty? lst) acc]
      [else(sum-list-tail(cdr lst)(+ acc(car lst)))])))
(sum-list-tail lst 0)

(define average
  (lambda (lst)
    (/(sum-list-tail lst 0)
      (size-tail lst 0))))
(average lst)

;; maximum-tail: list-of-number -> number
(define maximum-tail
  (lambda (lst best)
    (cond
      [(empty? lst) best]
      [(> (car lst) best) (maximum-tail (cdr lst)
                                        (car lst))]
      [else (maximum-tail (cdr lst) best)])))

(define list-of-numbers
  (lambda(start end)
    (cond
      [(eq? start end) (cons start '())]
      [else(cons start
                 (list-of-numbers (+ start 1) end))])))

(list-of-numbers 2 100)

(define evens
  (lambda(lst)
    (cond
      [(empty? lst)'()]
      [(=(remainder (car lst) 2) 0)
       (cons(car lst) (evens(cdr lst)))]
      [else(evens(cdr lst))])))
(evens lst)
