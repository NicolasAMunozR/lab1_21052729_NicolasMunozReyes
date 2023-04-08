#lang racket

(define login (lambda(system)
                (lambda(userN)
                  (if(not(null?(cdr(list-ref system 0))))
                     (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system))
                     (if(and(member userN (get-user system))(not(member userN (get-name system))))
                        (make-system(flatten(cons(get-name system)(make-user userN)))(get-drive system)
                                    (get-user system)(get-carpetas system))
                        (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system)))))))