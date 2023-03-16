#lang racket
(define a 5)
(define fn
  (lambda(x)
    (+ x 5)))
(define fn2
  (lambda (x y)
    (+ x y)))
(define fn3
  (lambda(x)
    (lambda(y)
      (+ x y))))
;; funcion suma de cuadrados
(define sum-of-square
  (lambda(x y)
    (+(* x x)(* y y))))
(sum-of-square 1 2)

;;area of a disk
(define area-of-disk
  (lambda(radius)
    (* 3.1415 radius radius)))
(area-of-disk 10)

;;area of a ring
(define area-of-ring
  (lambda(outer inner)
    (-(area-of-disk outer)
      (area-of-disk inner))))

;;wage : numer numer -> number
(define wage
  (lambda(payment hours)
    (* payment hours)))
(wage 12 2)

(define tax
  (lambda(wage rate)
  (* wage rate)))
(tax 100 0.15)

(define netpay
  (lambda(payment hours rate)
    (- (wage payment hours)
    (tax(wage payment hours)rate))))
(netpay 12 40 0.15)

(define maximum
  (lambda(a b)
    (cond
      [(> a b)a]
      [else b])))
(maximum 10 5)

(define interest
  (lambda(amount)
    (cond
      [(< amount 500)20]
      [(< amount 2000)90]
      [(< amount 10000)500])))
(interest 50)
(define factorial
  (lambda(n)
    (cond
      [(= 1 n)1]
      [else (* n(factorial(- n 1)))])))
(factorial 5)

(define sum
  (lambda(start end)
    (cond
      [(= end start)start]
      [else(+ end(sum start(- end 1)))])))

(sum 1 100)

(define gcd
  (lambda (a b)
    (cond
      [(= b 0)a]
      [else(gcd b(remainder a b))])))
