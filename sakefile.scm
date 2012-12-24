(define modules
  '(algorithm/conversion
    algorithm/comprehension
    algorithm/list
    algorithm/list-extra
    algorithm/random
    algorithm/stream
    algorithm/stream-extra
    algorithm/vector
    algorithm/stream-extra
    structure/multi-dimensional-array
    structure/stream))

(define-task compile ()
  (for-each (lambda (m)
              (sake:compile-c-to-o (sake:compile-to-c m))
              #;
              (sake:compile-c-to-o (sake:compile-to-c m
                                                      version: '(debug)
                                                      compiler-options: '(debug))))
            modules))

(define-task test ()
  (sake:test-all))

(define-task clean ()
  (sake:default-clean))

(define-task install ()
  (for-each (lambda (m)
              (sake:install-compiled-module m)
              #;(sake:install-compiled-module m version: '(debug))
              )
            modules)
  (sake:install-system-sphere))

(define-task uninstall ()
  (sake:uninstall-system-sphere)
  (delete-file prelude-system-path))

(define-task all (compile install)
  'all)
