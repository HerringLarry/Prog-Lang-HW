(define (accumulate op init seq)
  (cond ((null? seq) init)
        (else (op (car seq) (accumulate op init (cdr seq))))))

(define (accumulate-n op init seqs)
   (if (null? (car seqs))
       '()
       (cons (accumulate op init (map car seqs))
             (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v) (map (lambda (i) (dot-product i v)) m))

;(matrix-*-vector '((1 2) (3 4)) '(2 3))

(define (transpose mat) (accumulate-n (lambda (x y) (cons x y)) '() mat))
;(transpose '((1 2) (3 4)))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
(map  (lambda (i) (matrix-*-vector cols i)) m)))

;(matrix-*-matrix '((1 2) (3 4)) '((3 3) (2 2)))

(define (remove i ls)
  (cond ((null? ls) ls)
        ((= i (car ls)) (cdr ls))
        (else (cons (car ls) (remove i (cdr ls))))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (permutations ls)
  (cond ((null? ls) (list '()))
        (else (flatmap (lambda (i) (map (lambda (j) (cons i j)) (permutations (remove i ls)))) ls))))

;(permutations '(1 2 3 4))


(define tree1 '(A (
                   B
                   ()
                   (F
                    ()
                    ()
                    )
                   )
                  (C
                   ()
                   (G
                    (H
                     ()
                     ()
                     )
                    (I
                     (Z
                      ()
                      ()
                      )
                     ()
                     )
                    )
                   )
                  )
  )

(define tree2 '( A
                ( B
                  (D
                    (F
                      (Z
                         (V)
                         (O)
                       )
                      (N () ())
                     )
                    (G () ())
                   )
                  (E
                   (H () ())
                   (I () ())
                  )
                )
                ( C
                  ()
                  ()
                )
               )
  )


(define (find tr desired-depth item)
  (define (helper tree curr-depth)
    (cond ((null? tree) #f)
          ((and (= curr-depth desired-depth) (display (car tree)) (display item) (newline) (eq? (car tree) item) #t))
          ((> curr-depth desired-depth) #f)
          (else (or (helper (left-branch tree) (+ 1 curr-depth)) (helper (right-branch tree) (+ 1 curr-depth))))))
  (helper tr 0))

(define (left-branch tree)
   (cadr tree))
  
(cadr '(A () ()))


(define (right-branch tree)
  (cond ((null? (caddr tree)) '())
  (else (caddr tree))))


(define (atom? x)
  (and (not (null? x)) (not (pair? x))))

(define (depth tree)
  (cond ((null? tree) 0)
        ((atom? tree) 0)
        (else (max (+ 1 (depth (car tree))) (depth (cdr tree))))))

(depth tree1)

(define (enumerate x)
  (define (help i x)
    (cond ((> i x) '())
          (else (cons i (help (+ i 1) x)))))
  (help 0 x))

(define (BFT tree item)
  (define (help tr curr-depth max-depth result)
    (cond ((eq? result #t) #t)
          ((< max-depth curr-depth)#f)
          (else (help tr (+ curr-depth 1) max-depth (find tr curr-depth item)))))
  (help tree 0 (depth tree) #f))

(BFT tree1 'H)


;-------------------------------------------------------------------------------------------------------

(define l '(8 7 6 5 4 3 2 1))

(define (len x)
  (cond ((null? x) 0)
        (else (+ 1 (len (cdr x))))))

;Pre-Condition: ls1 and ls2 are sorted lists
;Post-Condition: Returns sorted list
;What to induct on: inducting on the lengths of the individual lists
;Base Case: If length of either list is 0 returns the other list
;Induction Hypothesis: The recursive calls work and at least one of the lists will reach length 0
;Induction Step: When the recursive call to merge is made, one of the two lists will be shortened. Assuming each list of finite length, one of the lists will reach length 0.
;Termination Step: It terminates once it gets reduced
(define (merge ls1 ls2)
  (cond ((null? ls1) ls2)
        ((null? ls2) ls1)
        ((> (car ls1) (car ls2)) (cons (car ls2) (merge ls1 (cdr ls2))))
        (else (cons (car ls1) (merge (cdr ls1) ls2)))))

(merge '(1 3 5) '())


;precondition: argument is a list of length > 0
;postcondition: returns second half of the list '(a b c d e f g) returns '(d e f g)
;Guess Invariant: original length = work-done + work-not-done
;Initialization: work-done = 0 and work-not-done = l
;Maintenance: work-done = l - work-not-done
;Termination: work-done = l // 2
;Does it terminate: yes because eventually i = stop because 1 is added to i before each iteration after the first

(define (first-half l)
  (define (help ls i stop)
    (cond ((< i stop) (cons (car ls) (help (cdr ls) (+ i 1) stop)))
          (else '())))
  (help l 0 (quotient (len l) 2)))

;precondition: argument is a list of length > 0
;postcondition: returns second half of the list '(a b c d e f g) returns '(d e f g)
;Guess Invariant: original length = work-done + work-not-done
;Initialization: work-done = 0 and work-not-done = l
;Maintenance: work-done = l - work-not-done
;Termination: work-done = l - l // 2


(define (second-half l)
  (define (help ls i stop)
    (cond ((= i stop) ls)
          (else (help (cdr ls) (+ i 1) stop))))
  (help l 0 (quotient (len l) 2)))


;precondition: argument is unsorted list
;postcondition: outputs sorted list

;Base Case: list of length 0 or list of length 1
;Inductive Hypothesis:
;  Recursive calls work correctly
;  Helper functions work correctly
;  the length of l which we call len is halved every cal;
;Inductive Step:
;  len // 2 = half of len
;  Thus the length is halved every recursive call eventually resulting in a list of length 0 or 1 depending on whether or not the list's original length is a power of 2
;  And since a list of length 0 or 1 is "sorted" the resulting list l-result will be sorted
;Termination COndition: If a list of finite length is passed as an argument then the function will eventually terminate because it is halved until
;its length is either 0 or 1 thus triggering the return condition

(define (merge-sort ls)
  (cond ((or (= (len ls) 0) (= (len ls) 1)) ls)
        (else (merge (merge-sort (first-half ls)) (merge-sort (second-half ls))))))

(merge-sort '(20 19 18 17 16 15 13 14 12 11 10 9 8 7 5 4 3 2 1 1 1 1 1))


;-----------------------------------------------------------------------------------------------


(define (make-boolean x)
  (cond ((= x 1) #t)
        (else #f)))

(define (make-and l1 l2)
  (list 'and l1 l2))

(define (make-or l1 l2)
  (list 'or l1 l2))

(define (make-not l1)
  (list 'not l1))

(define (get-operator lexp)
  (car lexp))

(define (get-first-operand lexp)
  (cadr lexp))

(define (get-second-operand lexp)
  (caddr lexp))

(define (lexp? s)
  (cond ((null? s) #f)
        ((atom? s) #f)
        ((and (= (len s) 3) (or (eq? (get-operator s) 'and) (eq? (get-operator s) 'or)))
         (and (check-operand? (get-first-operand s)) (check-operand? (get-second-operand s))))
        ((and (= (len s) 2) (eq? (get-operator s) 'not)) (check-operand? (get-first-operand s)))
        (else #f)))
         
(define (check-operand? op)
  (cond ((null? op) #f)
        ((list? op) (lexp? op))
        ((atom? op) #t)))
        
(lexp? '(and (or (and (not var1) var0) (or (and var0 var1) (or var0 var1))) (and (not var0) var1)))

(define (denestify lst)
  (cond ((null? lst) '())
        ((pair? (car lst))
         (append (denestify (car lst))
                 (denestify (cdr lst))))
        (else (cons (car lst) (denestify (cdr lst))))))

(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

(define (enum lexp)
  (filter not-operator? (denestify lexp)))

(define (not-operator? x)
  (cond ((eq? x 'and) #f)
        ((eq? x 'or) #f)
        ((eq? x 'not) #f)
        (else #t)))


(enum '(and (and var1 var2) (or var1 var4))) 

(define (is-in-help x los)
  (cond ((null? los) #f)
        ((eq? x (car los)) #t)
        (else (is-in-help x (cdr los)))))

(define (is-in? lst los)
  (cond ((null? lst) #t)
        (else (and (is-in-help (car lst) los) (is-in? (cdr lst) los)))))

(define (covered? lexp los)
  (cond ((null? lexp) #f)
        ((null? los) #f)
        ((lexp? lexp) (is-in? (enum lexp) los))
        (else #f)))






(define (lookup var alist)
  (cond ((null? alist) '())
        ((null? var) '())
        ((eq? var (caar alist)) (cadar alist))
        (else (lookup var (cdr alist)))))



(define (eval-operator op)
  (cond ((eq? 'and op) (lambda (x y) (and x y)))
        ((eq? 'or op) (lambda (x y) (or x y)))
        (else (lambda (x) (not x)))))

(define (evaluate sexp alist)
  (define (help s al)
    (cond ((= (len s) 2) ((eval-operator (get-operator s)) (eval-operand (get-first-operand s) al)))
          ((= (len s) 3) ((eval-operator (get-operator s)) (eval-operand (get-first-operand s) al) (eval-operand (get-second-operand s) al)))))
  (cond ((covered? sexp (first-only alist)) (help sexp alist))
        (else 0)))

(define (eval-operand op alist)
  (cond ((list? op) (evaluate op alist))
        (else (make-boolean (lookup op alist)))))
 

(define (first-only alist)
  (cond ((null? alist) '())
        (else (cons (caar alist) (first-only (cdr alist))))))

(evaluate '(or (and var1 var3)   var2 ) '((var1 1) (var2 1) (var3 0)))
    
         
        


        
;(and 1 0)
  
;(get-operator (make-and 1 1))








  
