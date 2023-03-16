#lang racket
;;; funcion fahrenheit to celsius
;;;entrada f en fahrenheit, y convierte a C
(define convert
  (lambda(f)
    (* (- f 32) (/ 5 9))
    ))

(convert 212.0)
(convert 32.0)
(convert -40.0)


;;;La funcion sign recibe como entrada un valor entero n. Devuelve -1 si n es negativo,
;;;1 si n es positivo mayor que cero, o 0 si n es cero

(define sign
  (lambda (n)
  (cond
   [(< n 0)-1]
      [(> n 0)1]
      [(= n 0)0])))

(sign -5)
(sign 10)
(sign 0)

;;;La funci ́on roots devuelve la ra ́ız que resuelve una ecuaci ́on
;;;cuadr ́atica a partir de sus tres coeficiente, a, b y
;;c, que se reciben como entrada. Se debe usar la siguiente f ́ormula:

(define roots
  (lambda (a b c)
    (/ (+ (- b) (sqrt (- (* b b) (* 4 (* a c))))) (* 2 a))))
    
(roots 2 4 2)
(roots 1 0 0)
(roots 4 5 1)

;;;La funci ́on bmi recibe dos entrada: weight y height.
;;;Debe devolver un s ́ımbolo que represente la descripci ́on
;;;del BMI correspondiente calculado a partir de sus entradas.

(define bmi
  (lambda (weight height)
  (cond
    [(< (/ weight (* height height)) 20) "underweight"]
    [(and (>= (/ weight (* height height)) 20)
         (< (/ weight (* height height)) 25)) "normal"]
    [(and (>= (/ weight (* height height)) 25)
         (< (/ weight (* height height)) 30))"obese1"]
    [(and (>= (/ weight (* height height)) 30)
         (< (/ weight (* height height)) 35)) "obese2"]
    [(>= (/ weight (* height height)) 40)"obese3"])))
(bmi 45 1.7)
(bmi 55 1.5)
(bmi 76 1.7)
(bmi 81 1.6)
(bmi 120 1.6)

;;;factorial

(define factorial
  (lambda(n)
    (cond
      [(= 1 n)1]
      [(= 0 n)1]
      [else (* n (factorial (- n 1)))])))

(factorial 0)
(factorial 5)
(factorial 40)

;;;6. duplicate
(define (duplicate lst)
  (append-map (lambda (ele) (list ele ele))
              lst))
(duplicate '())
(duplicate '(1 2 3 4 5))
(duplicate '(a b c d e f g h))

;;;7.pow
(define pow
  (lambda (a b)
    (cond
      [(= b 0)1]
      (else
       (* a (pow a (- b 1)))))))

(pow 5 0)
(pow -5 3)
(pow 15 12)

;;;8. fibonacci
(define fib
  (lambda (n)
    (cond
      [(<= n 1)n]
      (else
       (+ (fib (- n 1)) (fib (- n 2)))))))
(fib 6)
(map fib (range 10))
;;(fib 42)

;;;9. enlist
(define enlist
  (lambda (lst)
    (map list lst)))
(enlist '())
(enlist '(a b c))
(enlist '((1 2 3) 4 (5) 7 8))

;;;10. positives
(define positives
  (lambda (lst)
    (cond
      [(empty? lst) '()]
      [(> (car lst) 0) (cons (car lst) (positives(cdr lst)))]
      (else
       (positives(cdr lst))))))
(positives '())
(positives '(12 -4 3 -1 -10 -13 6 -5))
(positives '(-4 -1 -10 -13 -5))
;;; 11. add-list
(define add-list
  (lambda (lst)
    (cond
      [(empty? lst) 0]
      (else
      [+ (car lst) (add-list (cdr lst))]))))
(add-list '())
(add-list '(2 4 1 3))
(add-list '(1 2 3 4 5 6 7 8 9 10))

;;;12. invert-pairs
(define invert-pairs
  (lambda (lst)
    (cond
      [(empty? lst) '()]
      [else
       (cons (append (cdr (car lst))
                     (list (car (car lst))))
             (invert-pairs (cdr lst)))])))

(invert-pairs '())
(invert-pairs '((a 1)(a 2)(b 1)(b 2)))
(invert-pairs '((January 1)(February 2)(March 3)))
