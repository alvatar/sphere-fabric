;;; Copyright (c) 2005, Michael Sperber. All rights reserved.
;;; Copyright (c) 2012-2014, Alvaro Castro-Castilla. All rights reserved.
;;; Part of the API is implemented natively by Gambit

;;;!!! SRFI-66 Octet Vectors

(cond-expand
 (optimize
  (declare (standard-bindings) (extended-bindings) (not safe) (block)))
 (debug
  (declare (safe) (debug) (debug-location) (debug-source) (debug-environments)))
 (else (void)))

;;! Copies data from octet vector source to octet vector target. Source-start,
;; target-start, and n must be non-negative exact integers that satisfy
;; 0 <= source-start <= source-start + n <= (u8vector-length source)
;; 0 <= target-start <= target-start + n <= (u8vector-length target)
;; This copies the octets from source at indices [source-start, source-start + n)
;; to consecutive indices in target starting at target-index.
;; This must work even if the memory regions for the source and the target
;; overlap, i.e., the octets at the target location after the copy must be equal
;; to the octets at the source location before the copy.
;; The number of return values and the return values are unspecified.
;; However, the number of return values is such that it is accepted by a
;; continuation created by begin. Analogous to vector-ref.
;; .author Michael Sperber
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

;;! Returns #t if u8vector-1 and u8vector-2 are equal---that is, if they
;; have the same length and equal elements at all valid indices.
;; .author Michael Sperber
(define (u8vector=? u8vector-1 u8vector-2)
  (let ((size (u8vector-length u8vector-1)))
    (and (= size (u8vector-length u8vector-2))
	 (let loop ((i 0))
	   (or (>= i size)
	       (and (= (u8vector-ref u8vector-1)
		       (u8vector-ref u8vector-2))
		    (loop (+ 1 i))))))))

;;! Compares u8vector-1 and u8vector-2 and returns a value consistent with the
;; vector ordering specified in SRFI 67, i.e. -1 if u8vector-1 is smaller than
;; u8vector-2, 0 if they are equal, and 1 if u8vector-1 is greater than u8vector-2.
;; Shorter vectors are always smaller than longer ones, and vectors of equal length
;; are compared lexicographically.
;; .author Michael Sperber
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
