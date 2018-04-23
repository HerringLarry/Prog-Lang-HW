;Abelson and Sussman 2.2

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment p1 p2)
  (cons p1 p2))

(print-point (make-point 3 4))

(define (start-segment l)
  (car l))

(define (end-segment l)
  (cdr l))

(define (print-segment l)
  (print-point (start-segment l))
  (display ",")
  (print-point (end-segment l)))

(define x (make-point 3 4))
(define y (make-point 4 5))


(define (find-midpoint l)
  (cons (/ (+ (x-point (start-segment l)) (x-point (end-segment l))) 2) (/ (+ (y-point (start-segment l)) (y-point (end-segment l))) 2)))

(find-midpoint (make-segment x y))

(define (get-length l)
  (sqrt (+ (expt (- (x-point (end-segment l)) (x-point (start-segment l))) 2) (expt (- (y-point (end-segment l)) (y-point (start-segment l))) 2))))

;Abelson and Sussman 2.3


;First way to define rectangle (point, width and height)
(define (make-rectangle p1 l1 l2)
  (cons p1 (cons l1 l2)))

;Second way to define rectange (2 opposite points
(define (make-rectangle-4 p1 p2)
  (cons p1 p2))

(define (get-width rect)
  (car rect))

(define (get-height rect)
  (cdr rect))

(define (rect-width r)
   (abs (- (x-point (car r)) (x-point (cdr r)))))

(define (rect-height r)
   (abs (- (y-point (car r)) (y-point (cdr r)))))
  
(define (get-area rect)
  (* (get-width) (get-height l2)))

(define (get-perimeter rect)
  (+ (* 2 (get-width rect)) (* 2 (get-height l2))))

(get-length (make-segment (make-point 1 1) (make-point 4 5)))

;2.4 Substitution model 

(define (conss x y)
  (lambda (m) (m x y)))



(define (carr z)
  (z (lambda (p q) p)))

(carr (conss 1 2)) ; returns conss' lambda where z is and passes lambda in carr as m so ((lambda (p q) p) x y)

(define (cdrr z)
  (z (lambda (p q) q)))



