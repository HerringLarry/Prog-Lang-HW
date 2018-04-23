(define (add-value x y z)
  (cond ((and (and (= x 1) (= y 1)) (= z 1)) 1)
        ((and (= x 1) (= y 1)) 0)
        ((and (= x 1) (= z 1)) 0)
        ((and (= y 1) (= z 1)) 0)
        ((= x 1) 1)
        ((= y 1) 1)
        ((= z 1) 1)
        (else 0)))

;11 + 11 = 110

(define (carry-value x y z)
  (cond ((and (and (= x 1) (= y 1)) (= z 1)) 1)
        ((and (= x 1) (= y 1)) 1)
        ((and (= x 1) (= z 1)) 1)
        ((and (= y 1) (= z 1)) 1)
        (else 0)))


(define (decide-value lst)
  (cond ((null? lst) 0)
        (else (car lst))))

(define (decide-lst lst)
  (cond ((null? lst) '())
        (else (cdr lst))))

(define (remove-padded lst)
  (cond ((null? lst) '())
        ((= 0 (car lst)) (remove-padded (cdr lst)))
        (else lst)))

(define (add-base-1 lst1 lst2)
  (define (add-help lst1 lst2 carry)
    (cond ((and (null? lst1) (null? lst2)) (list (add-value 0 0 carry)))
          (else (cons (add-value (decide-value lst1) (decide-value lst2) carry) (add-help (decide-lst lst1) (decide-lst lst2) (carry-value (decide-value lst1) (decide-value lst2) carry))))))
  (remove-padded (reverse (add-help (reverse lst1) (reverse lst2) 0))))



(define (convert-to-b10 lst1)
  (define (help count lst init)
    (cond ((equal? lst init) count)
          (else (help (+ count 1) lst (add-base-1 '(1) init)))))
  (help 0 lst1 '(0)))

(define (multiply lst1 lst2)
  (define (help count lst total)
    (cond ((= count 0) total)
          (else (help (- count 1) lst (add-base-1 lst total)))))
  (help (convert-to-b10 lst1) lst2 '(0)))

(define (ex lst1 power)
  (define (help total pow)
    (cond ((= pow 1) total)
          (else (help (multiply total lst1) (- pow 1)))))
  (help lst1 (convert-to-b10 power)))


(define (atom? e)
  (and (not (pair? e)) (not (null? e))))


;(atom? '(+ 0 1 0 1))

(define (first-operand lst)
  (car lst))

(define (second-operand lst)
  (car (cdr (cdr lst))))

(define (operator lst)
  (car (cdr lst)))

;(first-operand '(1 + 2))
;(operator '(1 + 2))
;(second-operand '(1 + 2))

(define (plus-aexp? op)
  (cond ((eq? op '+) #t)
        (else #f)))

(define (times-aexp? op)
  (cond ((eq? op '*) #t)
        (else #f)))

(define (expt-aexp? op)
  (cond ((eq? op '^) #t)
        (else #f)))

(define (checker? e)
  (cond ((null? e) #t)
        ((atom? (car e)) #t)
        (else #f)))

(define (value e)
  (display e)
  (cond ((checker? e) e)
        ((plus-aexp? (operator e)) (add-base-1 (value (first-operand e))(value (second-operand e))))))
        

(define (flatmap proc init seq)
  (accumulate append init (map proc seq)))

(define (accumulate proc init seq)
  (cond ((null? seq) init)
        (else (proc (car seq) (accumulate proc init (cdr seq))))))


(value '((1 0 0) + (1 0 0)))

(define (filter pred seq)
  (cond ((null? seq) '())
    ((pred seq) (cons seq (filter pred (cdr seq))))
    (else (filter pred (cdr seq)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose 





 

           