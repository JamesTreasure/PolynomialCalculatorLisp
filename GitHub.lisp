;Function to collect like terms
(defun collect (l)
    ;extract first list, second list and the remainder
    (let ((m1(car l))
          (m2(car(cdr l)))
          (rem(cdr(cdr l))))
    
        ; If the list has less than 2 elements
        (if (null m2)
            ; then return the list (nothing to collect)                                                          
            l
            ; else, extract (c1, t1), (c2, t2) from m1 and m2                                                                 
            (let ((c1(car m1))                                                 
                  (c2(car m2))
                  (t1(car(cdr m1)))
                  (t2(car(cdr m2))))

                (if (equal t1 t2)
                    ; then return collect((c1 + c2, t1) + rem)
                    (collect (append (list (list (+ c1 c2) t1)) rem))
                    ; else return (c1, t1) + collect((c2, t2) + rem)          
                    (append (list m1) (collect (append (list m2) rem))))))))

;Function to compare variables against each other, such that a < b
(defun atom-less (a b)
  (cond
    ((null a) t)
    ((null b) t)
    ;if a is a number
    ((if (realp a)
      ;return a < b
      (< a b)
      ;else use string-lessp which compares lexicograhpically such that a < b, c < d, x < y etc
      (string-lessp a b)))))

;list less than function, returns true if first list is less than the second
(defun llt (a b)
 (cond
  ((null a) nil)
  ((atom a) (atom-less a b))
  ((cond
   ((llt (car a) (car b)) t)
   ((equal (car a) (car b)) (llt (cdr a) (cdr b)))
   (nil)))))

;compares two terms
(defun compare(a b)
  (llt (car (cdr a)) (car (cdr b))))

;sorts the terms so that the collect function can work.
(defun sortcollect(a)
  (sort a #'compare))

;Removes a term in a polynomial if the coefficient is 0
(defun my-remove(a)
  (if (null a)
    a
    ;if the coefficient is equal to 0
    (if (= (car(car a)) 0)
      ;recurse on my remove with the rest of the list
      (my-remove (cdr a))
      ;else add to the list and recurse on the remainder
      (append (list (car a)) (my-remove (cdr a))))))

;Function to negate a polynomial. This allows subtraction to implemented via additon.
(defun negate(a)
  ;Makes every coefficient in a polynomial negative
  (map 'list (lambda (x) (list (- (car x)) (car (cdr x)))) a))

;multiplies two terms together
(defun term-multiply(a b)
  (list (* (car a) (car b)) (collect(sortcollect(append (car(cdr a)) (car(cdr b)))))))

;multiplies a term by a polynomial
(defun term-polynomial (poly term) 
  (map 'list #'(lambda (x) (term-multiply x term)) poly))

;addition function
(defun p+(a b)
  (my-remove(collect(sortcollect(append a b)))))

;subtraction function
(defun p-(a b)
  (p+ a (negate b)))

;multiplication function
(defun p* (a b)
  (if (null b) 
    nil
    (p+ (term-polynomial a (car b)) (p* a (cdr b)))))

(print(p*(p- '((10((1 x)))(4((4 y)))) '((5((3 x)))(4((3 z))))) '((3((2 y))))))

