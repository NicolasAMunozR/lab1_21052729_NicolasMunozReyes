#lang racket

(define (make-let lett)(list lett))

(define switch-drive (lambda(system)(lambda(letter)
                                      (if(or(null?(cdr(get-name system)))(not(member letter(get-drive system))))
                                         (make-system(get-name system)(get-drive system)(get-user system))
                                         (make-system (flatten(cons(get-name system)(make-let letter)))(get-drive system)
                                         (get-user system))))))