#lang racket

(define (make-drive letra nombre capacidad)(list letra nombre capacidad))

(define add-drive (lambda(system) (lambda(letra nombre capacidad) 
                                  (let ((found (assoc letra system)))
                                    (if found
                                        system
                                        (cons (make-drive letra nombre capacidad) system))))))