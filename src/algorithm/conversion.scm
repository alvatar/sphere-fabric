;;; Copyright (c) 2012-2014, Alvaro Castro-Castilla. All rights reserved.
;;; Conversion between misc representations of data


(cond-expand
 (optimize
  (declare (standard-bindings) (extended-bindings) (not safe) (block)))
 (debug
  (declare (safe) (debug) (debug-location) (debug-source) (debug-environments)))
 (else (void)))


;;------------------------------------------------------------------------------
;;!! Inexact

;;! Normalized (0.0-1.0 range) to u8 integer
(define (normalized-inexact->integer value)
  (modulo (inexact->exact (round (* 255 value))) 255))


;;------------------------------------------------------------------------------
;;!! Bignum

(define (bignum->fixnum-list x radix-minus-1)
  (let* ((big-radix
          (+ radix-minus-1 1))
         (square-series
          (let loop ((square big-radix)
                     (square-list (list big-radix)))
            (let ((new-square
                   (* square square)))
              (if (< x new-square)
                  square-list
                  (loop new-square
                        (cons new-square square-list)))))))
    (define (convert n square-series tail)
      (if (pair? square-series)
          (let* ((q (quotient n (car square-series)))
                 (r (remainder n (car square-series)))
                 (new-square-series (cdr square-series)))
            (convert r
                     new-square-series
                     (convert q
                              new-square-series
                              tail)))
          (let ((d n))
            (if (and (null? tail) ;; avoid leading zeroes
                     (= d 0))
                tail
                (cons d tail)))))
    (convert x square-series '())))

(define (fixnum-list->bignum digit-list radix-minus-1)
  ;; Note: a divide-and-conquer algorithm would be faster for large numbers.
  (let ((big-radix (+ radix-minus-1 1)))
    (let loop ((n 0) (lst (reverse digit-list)))
      (if (pair? lst)
          (loop (+ (* n big-radix) (car lst))
                (cdr lst))
          n))))

(define (bignum->u8vector n)
  (list->u8vector (reverse (bignum->fixnum-list n 255))))

(define (u8vector->bignum u8vect)
  (fixnum-list->bignum (reverse (u8vector->list u8vect)) 255))
