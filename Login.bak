#lang racket

(define (make-us userN)(list userN))

(define login (lambda(system)(lambda(userN)
                               (if(not(null?(cdr(list-ref system 0))))
                                  (make-system(get-name system)(get-drive system)(get-user system))
                                  (if(and(member userN (get-user system))(not(member userN (get-name system))))
                                     (make-system(flatten(cons(get-name system)(make-us userN)))(get-drive system)
                                                 (get-user system))
                                     (make-system(get-name system)(get-drive system)(get-user system)))))))