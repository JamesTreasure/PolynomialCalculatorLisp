(defun collect (l)
    (let ((m1(car l))
          (m2(car(cdr l)))
          (rem(cdr(cdr l))))
    
        (if (null m2)                                                          ; If the list has less than 2 elements
            l                                                                  ; then return the list (nothing to collect)
            (let ((c1(car m1))                                                 ; else, extract (c1, t1), (c2, t2) from m1 and m2
                  (c2(car m2))
                  (t1(car(cdr m1)))
                  (t2(car(cdr m2))))

                (if (equal t1 t2)
                    (collect (append (list (list (+ c1 c2) t1)) rem))          ; then return collect((c1 + c2, t1) + rem)
                    (append (list m1) (collect (append (list m2) rem))))))))   ; else return (c1, t1) + collect((c2, t2) + rem)

(defun atom-less (a b)
  (cond
    ((null a) t)
    ((null b) t)
    ((if (realp a)
      (< a b)
      (string-lessp a b)
    ))
  )
)

(defun llt (a b)
 (cond
  ((null a) nil)
  ((atom a) (atom-less a b))
  ((cond
   ((llt (car a) (car b)) t)
   ((equal (car a) (car b)) (llt (cdr a) (cdr b)))
   (nil)))))

(defun compare(a b)
  (llt (car (cdr a)) (car (cdr b))))

(defun sortcollect(a)
  (sort a #'compare))

(defun p+(a b)
  (my-remove(collect(sortcollect(append a b)))))

(defun my-remove(a)
  (if (null a)
    a
    (if (= (car(car a)) 0)
      (my-remove (cdr a))
      (append (list (car a)) (my-remove (cdr a))))))

(defun negate(a)
  (map 'list (lambda (x) (list (- (car x)) (car (cdr x)))) a))

(defun p-(a b)
  (p+ a (negate b)))

(defun term-multiply(a b)
  (list (* (car a) (car b)) (collect(sortcollect(append (car(cdr a)) (car(cdr b)))))))

(defun term-polynomial (poly term) 
  (map 'list #'(lambda (x) (term-multiply x term)) poly))

(defun p* (a b)
  (if (null b) 
    nil
    (p+ (term-polynomial a (car b)) (p* a (cdr b)))))

(print(p* '((5((2 x)))(3((3 y)))) '((5((2 x)))(2((3 x))))))
;(print(term-polynomial '((5((2 x)))(3((2 y)))) '(5((2 x)))))

;so take the first term of the first polynomial, multiply it with the other polynomial, recurse on the remainder of the first polynomial and add these two result polynomials together
