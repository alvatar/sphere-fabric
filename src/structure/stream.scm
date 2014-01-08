;;!!! SRFI-41: Streams (primitives)
;; Copyright (C) Philip L. Bewig (2007). All Rights Reserved.
;; Modifications:
;; 2009 Kon Lovett
;; 2012-2014 Álvaro Castro-Castilla


(cond-expand
 (optimize
  (declare (standard-bindings) (extended-bindings) (not safe) (block)))
 (debug
  (declare (safe) (debug) (debug-location) (debug-source) (debug-environments)))
 (else))

;;!! Streams primitives

(define-record-type stream-type
  (make-stream promise)
  stream?
  (promise stream-promise stream-promise!))

(define (stream-eager expr)
  (make-stream
   (cons 'eager expr)))

(define (stream-force promise)
  (let ((content (stream-promise promise)))
    (case (car content)
      ((eager) (cdr content))
      ((lazy)  (let* ((promise* ((cdr content)))
                      (content  (stream-promise promise)))
                 (if (not (eqv? (car content) 'eager))
                     (begin (set-car! content (car (stream-promise promise*)))
                            (set-cdr! content (cdr (stream-promise promise*)))
                            (stream-promise! promise* content)))
                 (stream-force promise))))))

(define stream-null (stream-delay (cons 'stream 'null)))

(define-record-type stream-pare
  (make-stream-pare kar kdr)
  stream-pare?
  (kar stream-kar)
  (kdr stream-kdr))

(define (stream-pair? obj)
  (and (stream? obj) (stream-pare? (stream-force obj))))

(define (stream-null? obj)
  (and (stream? obj)
       (eqv? (stream-force obj)
             (stream-force stream-null))))

(define (stream-car strm)
  (cond ((not (stream? strm)) (error 'stream-car "non-stream"))
        ((stream-null? strm) (error 'stream-car "null stream"))
        (else (stream-force (stream-kar (stream-force strm))))))

(define (stream-cdr strm)
  (cond ((not (stream? strm)) (error 'stream-cdr "non-stream"))
        ((stream-null? strm) (error 'stream-cdr "null stream"))
        (else (stream-kdr (stream-force strm)))))
