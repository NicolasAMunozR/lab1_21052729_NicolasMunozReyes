#lang racket

(define logout (lambda (system)
                 (if(null?(cdr(get-name system)))
                    (make-system(get-name system)(get-drive system)(get-user system))
                    (make-system(cdr(cons(get-name system)(remove(list-ref(list-ref system 0)1)(get-name system))))
                                (get-drive system)(get-user system)))))
