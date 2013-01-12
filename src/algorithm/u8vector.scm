;;!!! SRFI-66 Octet Vectors

(declare
 (block)
 (standard-bindings)
 (extended-bindings)
 (not safe))

;; The rest of the API is compatible with SRFI-4 (native in Gambit)

(define (u8vector-copy! source source-start target target-start count)
  (if (>= source-start target-start)
      (do ((i 0 (+ i 1)))
	  ((= i count))
        (u8vector-set! target
                       (+ target-start i) 
                       (u8vector-ref source (+ source-start i))))
      (do ((i (- count 1) (- i 1)))
	  ((= i -1))
        (u8vector-set! target
                       (+ target-start i) 
                       (u8vector-ref source (+ source-start i))))))

(define (u8vector=? u8vector-1 u8vector-2)
  (let ((size (u8vector-length u8vector-1)))
    (and (= size (u8vector-length u8vector-2))
	 (let loop ((i 0))
	   (or (>= i size)
	       (and (= (u8vector-ref u8vector-1)
		       (u8vector-ref u8vector-2))
		    (loop (+ 1 i))))))))

(define (u8vector-compare u8vector-1 u8vector-2)
  (let ((length-1 (u8vector-length u8vector-1))
        (length-2 (u8vector-length u8vector-2)))
    (cond
     ((< length-1 length-2) -1)
     ((> length-1 length-2)  1)
     (else
      (let loop ((i 0))
        (if (= i length-1)
            0
            (let ((elt-1 (u8vector-ref u8vector-1 i))
                  (elt-2 (u8vector-ref u8vector-2 i)))
              (cond ((< elt-1 elt-2) -1)
                    ((> elt-1 elt-2)  1)
                    (else (loop (+ i 1)))))))))))
