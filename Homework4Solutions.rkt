;Recursive - Returns length of list
(define (my-length lst)
  (cond ((null? lst)
         0)
        (else (+ 1 (my-length(cdr lst))))))

;Iterative - Returns length of list
(define (my-length-iter lst)
  (define (iter count)
    (cond ((null? lst) count)
          (else (set! lst (cdr lst)) (iter (+ 1 count)))))
  (iter 0))

(my-length '(1 2 3 4))
(my-length-iter '(1 2 3 4))

;Recursive - Returns kth item of list
(define (list-ref-new lst k)
  (cond ((= k 0) (car lst))
        (else (list-ref-new (cdr lst) (- k 1)))))

;Iterative - Returns kth item of list
(define (list-ref-iter lst k)
  (define (iter-help count)
    (cond ((= count k) (car lst))
          (else (set! lst (cdr lst)) (iter-help (+ count 1)))))
  (iter-help 0))

(list-ref-new '(1 2 3) 1)
(list-ref-iter '(2 3 234 4 5) 2)

;Recursive - Takes two arguments lst and num and returns the first num elements of the lst
(define (ret-first lst num)
  (cond ((= num 1) (list(car lst)))
        (else (append (list (car lst)) (ret-first (cdr lst) (- num 1))))))

;Iterative - ""
(define (ret-first-iter lst num)
  (define (ret-first-help new-lst count)
    (cond ((= count num)   new-lst)
          (else (set! new-lst (append new-lst (list (car lst)))) (set! lst (cdr lst))  (ret-first-help new-lst (+ count 1)))))
  (ret-first-help '() 0))

(ret-first '(1 2 3 4 5) 3)

(ret-first-iter '(1 2 3 4) 3)


;Recursive - Takes two arguments lst and num and returns all but the last num elements of the list
(define (all-but lst num)
  (define (all-but-help lst num-inverse)
    (cond ((= num-inverse 0) (list(car lst)))
          (else (append (list(car lst)) (all-but-help (cdr lst) (- num-inverse 1))))))
  (all-but-help lst (- (- (my-length lst) 1) num)))

;Iterative - Takes two arguments lst and num and returns all but the last num elements of the list
(define (all-but-iter lst num)
  (define (all-help new-lst count lim)
    (cond ((= count lim) new-lst)
          (else (set! new-lst (append new-lst (list (car lst)))) (set! lst (cdr lst)) (all-help new-lst (+ count 1) lim))))
  (all-help '() 0 (- (my-length lst) num)))

(all-but-iter '(1 2 3 4) 3)

(all-but '(1 2 3 4 5) 2)

;Recursive - Takes two arguments lst and num and returns the last num elements of the list

(define (last-num lst num)
  (define (last-help ls inv len)
    (cond ((and (= inv 0) (> len 0)) (append (list (car ls)) (last-help (cdr ls) inv (- len 1))))
          ((= inv 0) '())
          (else (last-help (cdr ls) (- inv 1) (- len 1)))))
  (last-help lst (- (my-length lst) num) (my-length lst)))

(last-num '(1 2 3 4 5) 3)
  