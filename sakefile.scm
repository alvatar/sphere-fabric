(define modules
  '(algorithm/compare
    algorithm/comprehension
    algorithm/conversion
    algorithm/list
    algorithm/list-extra
    ;; algorithm/random
    algorithm/sort-merge
    algorithm/stream
    algorithm/stream-extra
    algorithm/u8vector
    algorithm/vector
    structure/array
    structure/multi-dimensional-array
    structure/srfi-69-table
    structure/stream))

;; (define modules
;;   '(algorithm/sort-merge))

(define-task compile ()
  (for-each (lambda (m) (sake#compile-c-to-o (sake#compile-to-c m compiler-options: '(debug)))) modules)
  (for-each (lambda (m) (sake#compile-c-to-o (sake#compile-to-c m))) modules))

(define-task test ()
  (sake#test-all))

(define-task clean ()
  (sake#default-clean))

(define-task install ()
  (for-each sake#install-compiled-module modules)
  (sake#install-sphere-to-system))

(define-task uninstall ()
  (sake#uninstall-sphere-from-system))

(define-task all (compile install)
  'all)
