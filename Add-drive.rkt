#lang racket

(define get-name car)

(define get-drive cadr)

(define get-user caddr)

(define (make-drive letra nombre capacidad)(list letra nombre capacidad))

(define add-drive (lambda(system)
                    (lambda(letra nombre capacidad)
                      (if(null? (list-ref system 1))
                         (make-system (get-name system)(cons(make-drive letra nombre capacidad)(get-drive system))
                                      (get-user system))
                         (if(member letra (flatten(list-ref system 1)))
                            (make-system(get-name system)(get-drive system)(get-user system))
                            (make-system (get-name system)(flatten(cons(get-drive system)(make-drive letra nombre capacidad)))
                                         (get-user system)))))))
