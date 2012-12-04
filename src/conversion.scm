;;; Copyright (c) 2012, Alvaro Castro-Castilla. All rights reserved.
;;; Data conversion procedures

;;; 0.0-1.0 range to u8 integer

(define (normalized-inexact->integer value)
  (modulo (inexact->exact (round (* 255 value))) 255))
