;;; Copyright (c) 2012, Alvaro Castro-Castilla. All rights reserved.
;;; List randomization and related algorithms

;;; Copyright (c) Taylor Cambpell. Public domain.
;;; (binary, merge, insertion, selection, Fisher-Yates)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shuffling and Reservoir Sampling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cond-expand
 (debug (declare (block)
                 (standard-bindings)
                 (extended-bindings)))
 (else (declare (block)
                (standard-bindings)
                (extended-bindings)
                (not safe))))

;;;=============================================================================

;;; Provides procedures to get random bits of high quality.

(package* random/v1.0.0
 (provide:

  (define* (make-random-u8vector len))
  (define* (random-bignum range))
  (define* (random-fixnum range)))

 (maintainer:
  "Scheme Now! <snow at iro.umontreal.ca>")

 (author:
  "Marc Feeley <feeley at iro.umontreal.ca>")

 (homepage:
  "http://snow.iro.umontreal.ca")

 (description:
  "High-quality random number generation.")

 (keywords: snow)

 (license: lgpl/v2.1)

 (require: homovector/v1)
 (require: binio/v1)
 (require: bignum/v1))

;;;=============================================================================

(cond-expand

 ((and srfi-27
       (not gauche) ;; work around a bug in Gauche
       (not stklos)) ;; work around a bug in STklos

  (define* (make-random-u8vector len)
    (let* ((rs (make-random-source))
           (mi (random-source-make-integers rs))
           (u8vect (snow-make-u8vector len)))
      (random-source-randomize! rs)
      (let loop ((i 0))
        (if (< i len)
            (begin
              (snow-u8vector-set! u8vect i (mi 256))
              (loop (+ i 1)))))
      u8vect)))

 (else

  (define-macro* (random-bits-file) "/dev/random")

  (define* (make-random-u8vector len)
    (let* ((in (binio-open-input-file (random-bits-file)))
           (u8vect (snow-make-u8vector len))
           (n (binio-read-subu8vector u8vect 0 len in)))
      (binio-close-input-port in)
      (if (= n len)
          u8vect
          (snow-error "random bits file ended prematurely"))))))

;;;----------------------------------------------------------------------------

(define* (random-bignum range)
  (let* ((range-bits (bignum-integer-length range))
         (len (quotient (+ range-bits 20) 8))
         (n (bignum-expt (fixnum->bignum 256) (fixnum->bignum len)))
         (divisor (bignum-quotient n range))
         (limit (bignum* divisor range)))
    (let loop ()
      (let* ((u8vect (make-random-u8vector len))
             (x (fixnum-list->bignum (snow-u8vector->list u8vect) 255)))
        (if (bignum>= x limit)
            (loop)
            (bignum-quotient x divisor))))))

(define* (random-fixnum range)
  (bignum->fixnum (random-bignum (fixnum->bignum range))))

;;;=============================================================================





(define (flip-coin) (if (= 1 (random-integer 2)) #t #f))

;-------------------------------------------------------------------------------
; Shuffle
;-------------------------------------------------------------------------------

;;! Binary Shuffle
;; Go through the list, collecting a left list and a right list by
;; randomly choosing which list to put successive elements on.
;; Recursively the left and right lists, and then concatenate them.
(define (binary-shuffle-list list)
  (define (bifurcate list left right)
    (if (null-list? list)
        (values left right)
        (let ((item (car list))
              (list (cdr list)))
          (if (flip-coin)
              (bifurcate list (cons item left) right)
              (bifurcate list left (cons item right))))))
  (let shuffle ((list list) (tail '()))
    (cond ((null-list? list)
           tail)
          ((null-list? (cdr list))
           (cons (car list) tail))
          ((null-list? (cddr list))
           (if (flip-coin)
               (cons (car list) (cons (cadr list) tail))
               (cons (cadr list) (cons (car list) tail))))
          (else
           (receive (left right) (bifurcate list '() '())
                    (shuffle left (shuffle right tail)))))))

(define (binary-shuffle-list! list)
  (define (bifurcate! list left right)
    (if (null-list? list)
        (values left right)
        (let ((item (car list))
              (next (cdr list)))
          (if (flip-coin)
              (begin (set-cdr! list left)
                     (bifurcate! next list right))
              (begin (set-cdr! list right)
                     (bifurcate! next left list))))))
  (let shuffle! ((list list) (tail '()))
    (cond ((null-list? list)
           tail)
          ((null-list? (cdr list))
           (set-cdr! list tail)
           list)
          ((null-list? (cddr list))
           ;; LIST is (A B), so...
           (if (flip-coin)
               (let ((next (cdr list)))
                 ;; ...set it to (B A . tail).
                 (set-cdr! list tail)
                 (set-cdr! next list)
                 next)
               (begin
                 ;; ...set it to (A B . tail).
                 (set-cdr! (cdr list) tail)
                 list)))
          (else
           (receive (left right) (bifurcate! list '() '())
                    (shuffle! left (shuffle! right tail)))))))

;;! Merge Shuffle
;; Partition the list into two equal halves; shuffle the two halves,
;; and then merge them by randomly choosing which half to select the
;; next element from.

(define (merge-shuffle-list list)
  (define (merge a b)
    (cond ((not (pair? a)) b)
          ((not (pair? b)) a)
          (else
           (if (flip-coin)
               (cons (car a) (merge (cdr a) b))
               (cons (car b) (merge a (cdr b)))))))

  (define (partition list a b)
    (let ((next (cdr list))
          (a b)
          (b (cons (car list) a)))
      (if (null-list? next)
          (values a b)
          (partition next a b))))

  (if (null-list? list)
      '()
      (let shuffle ((list list))
        (if (null-list? (cdr list))
            list
            (receive (a b) (partition list '() '())
                     (merge (shuffle a) (shuffle b)))))))

(define (merge-shuffle-list! list)
  (define (merge! a b)
    (cond ((null-list? a)       b)
          ((null-list? b)       a)
          ((flip-coin)          (%merge! a b) a)
          (else                 (%merge! b a) b)))
  (define (%merge! a b)
    (cond ((null-list? (cdr a))
           (set-cdr! a b))
          ((flip-coin)
           (%merge! (cdr a) b))
          (else
           (%merge! b (let ((next (cdr a)))
                        (set-cdr! a b)
                        next)))))
  (define (partition! list a b)
    (let ((next (cdr list)))
      (set-cdr! list a)
      (if (null-list? next)
          (values list b)
          (partition! next b list))))
  (if (null-list? list)
      '()
      (let shuffle! ((list list))
        (if (null-list? (cdr list))
            list
            (receive (a b) (partition! list '() '())
                     (merge! (shuffle! a) (shuffle! b)))))))

;;! Insertion Shuffle
(define (insertion-shuffle-list list)
  (define (insert list position item)
    (if (zero? position)
        (cons item list)
        (cons (car list)
              (insert (cdr list) (- position 1) item))))
  (if (null-list? list)
      '()
      (let loop ((in (cdr list)) (count 1) (out (cons (car list) '())))
        (let ((count (+ count 1))
              (item (car in))
              (next (cdr in)))
          (let ((out (insert out (random-integer count) item)))
            (if (null-list? next)
                out
                (loop next count out)))))))

(define (insertion-shuffle-list! list)
  (define (insert! list lag position cell)
    (let ((position (- position 1)))
      (if (zero? position)
          (begin (set-cdr! lag cell)
                 (set-cdr! cell list))
          (insert! (cdr list) list position cell))))
  (if (null-list? list)
      '()
      (let ((in (cdr list)))
        (set-cdr! list '())
        (let loop ((in in) (count 1) (out list))
          (if (null-list? in)
              out
              (let ((next (cdr in))
                    (count (+ count 1)))
                (loop next
                      count
                      (let ((position (random-integer count)))
                        (if (zero? position)
                            (begin (set-cdr! in out)
                                   in)
                            (begin (insert! (cdr out) out position in)
                                   out))))))))))

;;; Selection Shuffle

(define (selection-shuffle-list list)
  (define (select list position)
    (if (zero? position)
        (values (car list) (cdr list))
        (receive (item tail)
                 (select (cdr list) (- position 1))
                 (values item (cons (car list) tail)))))
  (if (null-list? list)
      '()
      (let loop ((in list) (out '()) (len (length list)))
        (receive (item list) (select in (random-integer len))
                 (let ((out (cons item out)))
                   (if (null-list? list)
                       out
                       (loop list
                             (cons item out)
                             (- len 1))))))))

(define (selection-shuffle-list! list)
  (define (select! list lag position)
    (if (zero? position)
        (begin (set-cdr! lag (cdr list))
               list)
        (select! (cdr list) list (- position 1))))
  (if (null-list? list)
      '()
      (let loop ((in list) (out '()) (len (length list)))
        (let ((position (random-integer len)))
          (receive (cell next)
                   (if (zero? position)
                       (values in (cdr in))
                       (values (select! (cdr in) in (- position 1))
                               in))
                   (set-cdr! cell out)
                   (if (null-list? next)
                       cell
                       (loop next cell (- len 1))))))))

;;; Fisher-Yates O(n) Random-Access Shuffle

(define (Fisher-Yates-shuffler sequence-exchange!)
  (lambda (sequence start end)
    (do ((i start (+ i 1)))
        ((>= i end))
      (let ((j (+ start (random-integer (+ 1 (- i start))))))
        (if (not (= i j))
            (sequence-exchange! sequence i j))))))

(define (sequence-exchanger sequence-ref sequence-set!)
  (lambda (sequence i j)
    (let ((elt-i (sequence-ref sequence i))
          (elt-j (sequence-ref sequence j)))
      (sequence-set! sequence j elt-i)
      (sequence-set! sequence i elt-j))))

(define shuffle-vector!
  (Fisher-Yates-shuffler (sequence-exchanger vector-ref vector-set!)))

(define shuffle-string!
  (Fisher-Yates-shuffler (sequence-exchanger string-ref string-set!)))

;-------------------------------------------------------------------------------
; Random picking
;-------------------------------------------------------------------------------

;;; Pick a random element with known list length

(define (pick-random/length l len)
  (list-ref l (random-integer len)))

;;; Pick a random element

(define (pick-random l)
  (pick-random/length l (length l)))

;;; Pick a number of random elements without repetition

(define (pick-random//repetition l n)
  (let recur ((l l)
              (n n)
              (picked '()))
    (if (or (null? l) (zero? n))
        picked
        (receive (fore aft)
                 (split-at l (random-integer (length l)))
                 (recur (append fore (cdr aft))
                        (-- n)
                        (cons (car aft) picked))))))

;;; Pick a random element and return also the list without that element

(define (pick-random+rember/length l len)
  (receive (a b)
           (split-at l
                     (random-integer len))
           (values
            (car b)
            (append a (cdr b)))))

;;; Pick a random element and return also the list without that element

(define (pick-random+rember l)
  (pick-random+rember/length l (length l)))

;-------------------------------------------------------------------------------
; Reservoir sampling
;-------------------------------------------------------------------------------

;;; Naïve implementation of reservoir sampling. It shuffles the list and picks
;;; the top n as the reservoir.
;;; TODO: A better algorithm would build a list of length n filling it with
;;; randomly picked elements
;;; 

(define (reservoir-sampling shuffler lis n)
  (take (shuffler lis) n))
;; (require srfi/27)
;; (require srfi/41)
;; (define (random-sample size input)
;;   (let ((first-part (stream-take size input))
;;         (second-part (stream-drop size input))
;;         (pool (make-vector size)))
;;     (stream-for-each (match-lambda ((list i val)
;;                                     (vector-set! pool i val)))
;;                      (stream-zip (stream-from 0) first-part))
;;     (stream-for-each (match-lambda ((list i val)
;;                                     (let ((random-index (random-integer i)))
;;                                       (when (< random-index size)
;;                                         (vector-set! pool random-index val)))))
;;                      (stream-zip (stream-from size) second-part))
;;     (vector->list pool)))


; Select a uniformly distributed random node from a tree in one pass,
; without an a priori knowledge on the number of nodes in the tree.
;
; The algorithm is an instance of a Reservoir sampling:
; Select at random N records from a sequence -- without a
; priori knowledge of the total number of records in a sequence
; (provided that this number is greater than N). The method guarantees
; that each record is selected with a probability of N/M, where M is the
; total number of the records in the sequence.
;
; See
; Reservoir Sampling
; by Paul F. Hultquist and William R. Mahoney
; Dr. Dobbs J., January 2001, p. 189.
; The "Algorithm Alley" column.
; The algorithm was originally developed by Alan Waterman.
; 
; Article posting headers:
;   Date: Tue, 15 Apr 2003 22:17:15 -0700
;   Newsgroups: comp.lang.scheme
;   Subject: Re: random node in tree
;   References: <mYZma.4116$8T6.318252@news20.bellglobal.com>
;   Message-ID: <7eb8ac3e.0304152117.3124c038@posting.google.com>
;
; $Id: random-tree-node.scm,v 1.1 2003/04/17 02:13:43 oleg Exp oleg $


;   procedure random-node TREE -> [COUNT NODE]
; Traverse the TREE _once_ and return COUNT, the total number of nodes
; in the tree, and NODE -- one node of the tree selected with
; the probability of 1/COUNT. TREE must be a pair.
;
; We consider '() to be the absence of a child. Thus
; '(a . ()) is a tree with one, left child.
; '(() . a) is a tree with one right child.
; We do not count leaves as nodes. Only internal nodes (aka, pairs)
; count. If we wish to count leaves too (which are atoms other than
; '()), replace "(if (not (pair? node)) ...)" test below with
; "(if (null? node) ...)".

; We assume that a procedure (random i j) returns a random integer k,
; i <= k <= j, that is uniformly distributed within the interval [i,j]
; (endpoints included!)

(define (random-node tree)
  (let select ((node  (car tree)) (p 1) (reservoir tree)
	       (todo (list (cdr tree))))
    (if (not (pair? node))			; Leaves don't count
      (if (null? todo) (values p reservoir)
	(select (car todo) p reservoir (cdr todo)))
    (let*
      ((p (+ 1 p))
       (k (random 1 p))
       (reservoir (if (= k 1) node reservoir)))
      (if (pair? node)
	(select (car node) p reservoir (cons (cdr node) todo))
	(if (null? todo) (values p reservoir)
	  (select (car todo) p reservoir (cdr todo))))))))


; Proof of the algorithm.
;
; Claim:
; At each invocation of 
;	(select node p reservoir todo)
; p>=1 is the number of previously traversed nodes (up to but not including
;   'node')
; 'node' is either '() or the the (p+1)-th node of the tree
; reservoir is the node randomly selected from the traversed
;     with the probability 1/p
; todo is the stack of right brothers of 'node'

; Proof by induction:
;
; Base case: initial invocation.
; 'node' is the left child of the root or '(), todo a singleton list
; that contains its right brother, p = 1 and reservoir is the traversed
; node (which is the root).
; Claim holds.
;
; Induction hypothesis: Claim holds after q nodes are traversed, 
; as we enter (select node q reservoir todo)
; If 'node' does not count (it's '() or a leaf, if we don't count leaves)
; we skip it and continue the pre-order traversal. 
; If 'node' is the node that counts, we set q' to q+1, k to be a
; number uniformly distributed 1 <= k <= q'. The number k has the probability
; 1/q' = 1/(q+1) of being 1.
; We set reservoir to be 'node' with the probability 1/(q+1),
; we maintain the current value of the reservoir with the probability
; 1 - 1/(q+1) = q/(q+1). Thus reservoir is one of the q previously
; traversed nodes selected with the probability 1/q * q/(q+1) = 1/(q+1).
; If node has children, we recursively enter select, with
; the first argument being the left child of 'node' or nil, 
; the second argument (q+1) -- the number of nodes traversed,--
; reservoir is the node selected uniformly at random from the traversed,
; todo is the (grown) stack of the right brothers of the first argument.
; The claim holds.
; If 'node' is not a pair but 'todo' is not empty, we re-enter
; select but the invariant holds. If we don't count leaves as nodes,
; the latter alternative does not apply.
;
; It follows from the claim that when 'select' exits,
; it returns the total number of nodes in the tree and one node
; uniformly randomly selected from the them.


; Tests

; In the following we define (random i j) to always return its left
; boundary.
(define (random i j) i)

; With such a definition of "random", (random-node tree) will return
; the last traversed node.

(define (test-random-node tree)
  (display "Tree: ") (display tree) (newline)
  (call-with-values 
    (lambda () (random-node tree))
    (lambda (count rand-node)
      (display "total count: ") (display count)
      (display " selected node: ") (display rand-node)
      (newline))))

(test-random-node '(a))
(test-random-node '(() . (b c)))
(test-random-node '(() . (b . c)))
(test-random-node '(* (+ 3 4) 5))

; Results of the test runs
; Tree: (a)
; total count: 1 selected node: (a)
; Tree: (() b c)
; total count: 3 selected node: (c)
; Tree: (() b . c)
; total count: 2 selected node: (b . c)
; Tree: (* (+ 3 4) 5)
; total count: 6 selected node: (5)

