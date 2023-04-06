#lang racket

(define (make-user use) (list use))

(define register (lambda(system)
                   (lambda(use)
                     (if(null? (list-ref system 2))
                        (make-system(get-name system)(get-drive system)(flatten(cons(get-user system)(make-user use))))
                        (if (member use (flatten(list-ref system 2)))
                            (make-system(get-name system)(get-drive system)(get-user system))
                            (make-system(get-name system)(get-drive system)(flatten(cons(get-user system)(make-user use)))))))))