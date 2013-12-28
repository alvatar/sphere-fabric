(define modules
  '(algorithm/compare
    algorithm/comprehension
    algorithm/conversion
    algorithm/list
    algorithm/list-extra
    ;; algorithm/random TODO!!
    algorithm/sort-merge
    algorithm/stream
    algorithm/stream-extra
    algorithm/u8vector
    algorithm/vector
    structure/array
    structure/multi-dimensional-array
    structure/srfi-69-table
    structure/stream))

(define-task compile ()
  (for-each (lambda (m) (sake#compile-module m compiler-options: '(debug))) modules)
  (for-each sake#compile-module modules))

(define-task post-compile ()
  (for-each (lambda (m) (sake#make-module-available m versions: '(() (debug)))) modules))

(define-task install ()
  (sake#install-sphere-to-system))

(define-task test ()
  (sake#test-all))

(define-task clean ()
  (sake#default-clean))

(define-task all (compile post-compile)
  'all)
