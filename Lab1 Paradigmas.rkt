#lang racket

(require "TDAs_21052729_MunozReyes.rkt")

(provide (all-defined-out))

;Función System
;Dominio: Nombre del sistema, tipo de dato string
;Recorrido: Arroja el sistema creado
;LO que hace esta función es que crea el sistema, dejando el espacio necesario para agregar el nombre del sistema, las unidades (drive) que sean ingresadas,
;los usuaros que sean registrados, las carpetas y archivos que sean creados, una papelera donde dejar las cosas que sean eliminadas y la fecha de creación del sistema

;Coloco el name (Nombre del sistema, el cual sera ingresado) con un cons a un null, para definirlo como lista y colocar cosas luego, junto al nombre del sistema
(define (system name)(make-system(cons(string-upcase name)null) null null null null(fechayhora current-seconds)))

;---------------------------------------------------------------------------------------------------

;Función Run
;Dominio: El sistema y el comando a ejecutar (cualquiera de las funciones descritas posteriormente)
;Recorrido: Arroja el sistema luego de ejecutar la funcion y ser modificado
;Lo que hace la función es que da el pie para que puedan agregarse cosas al sistema y para que este mismo funcione

(define (run system comando)(comando system))

;---------------------------------------------------------------------------------------------------

;Función Add-drive
;Dominio: El sistema junto a la letra de la unidad drive (tipo char), el nombre de esta (tipo string) y la capacidad de esta (tipo int)
;Recorrido: Arroja el sistema luego de ejecutar la funcion y ser modificado
;Esta función permite añadir unidades (drive) al sistema, donde luego se crearan las carpetas y archivos

(define add-drive (lambda(system)(lambda(letra nombre capacidad)
                    (if(null? (list-ref system 1))
                       ;Reviso si es que el espacio destinado para las unidades (drive) esta vacio
                       (make-system (get-name system)(flatten(cons(make-drive letra nombre capacidad)(get-drive system)))(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                       ;Si es asi devuelvo toda la estructura del sistema añadiendole los elementos ingresados por comando al get-drive
                       (if(verificar(flatten(list-ref system 1))(car(string->list(string-upcase(string letra)))))
                          ;Si no se cumple el if anterior entonces ocupo la funcion verificar con el espacio destinado para las unidades (drive) y la letra del ingresada (tipo char)
                          (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                          ;Si devuelve true entonces devuelvo el sistema como estaba, ya que no pueden entrar unidades (drive) con la misma letra (tipo char)
                          (make-system (get-name system)(flatten(cons(get-drive system)(make-drive letra nombre capacidad)))(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system)))))))
                          ;Pero si devuelve false entonces devuelvo toda la estructura del sistema añadiendole los elementos ingresados por comando al get-drive

;--------------------------------------------------------------------------------------------------

;Función Register
;Dominio: El sistema junto al nombre del usuario que quiere registrarse (tipo string) 
;Recorrido: Arroja el sistema luego de ejecutar la funcion y ser modificado
;Esta funcion permite añadir usuarios al sistema, a los que luego les dejara iniciar o cerrar secion

(define register (lambda(system)(lambda(use)
                     (if(null? (list-ref system 2))
                        ;Reviso si es que el espacio destinado para usuarios registrados esta vacio
                        (make-system(get-name system)(get-drive system)(flatten(cons(get-user system)(string-upcase use)))(get-carpetas system)(get-papelera system)(get-fecha system))
                        ;Si es asi devuelvo toda la estructura del sistema añadiendole el elemento ingresado por comando al get-user
                        (if(verificar(flatten(list-ref system 2))(string-upcase use))
                            ;Si no se cumple el if anterior entonces ocupo la funcion verificar con el espacio destinado para usuarios registrados y el nombre ingresado (tipo string) 
                            (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                            ;Si devuelve true entonces devuelvo el sistema como estaba, ya que no pueden entrar usuarios repetidos (tipo string)
                            (make-system(get-name system)(get-drive system)(flatten(cons(get-user system)(string-upcase use)))(get-carpetas system)(get-papelera system)(get-fecha system)))))))
                            ;Pero si devuelve false entonces devuelvo toda la estructura del sistema añadiendole el elemento ingresado por comando al get-user

;--------------------------------------------------------------------------------------------------

;Función Login
;Dominio: El sistema junto al nombre del usuario que quiere iniciar secion (tipo string) 
;Recorrido: Arroja el sistema luego de ejecutar la funcion y ser modificado
;Esta función permite que un usuario que este registrados pueda iniciar secion

(define login (lambda(system)(lambda(userN)
                            (if(not(null?(cdr(list-ref system 0))))
                               ;Reviso si es que en la lista donde se encuentra el nombre del sistema el segundo elemnto no es nulo
                               (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                               ;Si devuelve true entonces devuelvo el sistema como estaba, ya que no pueden iniciar secion dos usuarios a la vez
                               (if(verificar-str(get-user system)userN)
                                  ;Si no se cumple el if anterior entonces ocupo la funcion verificar-str con el espacio destinado para usuarios registrados y el nombre ingresado por comando (tipo string)
                                  (make-system(flatten(cons(get-name system)(string-upcase userN)))(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                                  ;Si devuelve true entonces devuelvo toda la estructura del sistema añadiendole el elemento ingresado por comando a la segunda posicion del get-name
                                  (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system)))))))
                                  ;Pero si devuelve false entonces devuelvo el sistema como estaba, ya que no pueden iniciar secion usuarios que no esten registrados

;--------------------------------------------------------------------------------------------------

;Función Logout
;Dominio: El sistema  
;Recorrido: Arroja el sistema luego de ejecutar la funcion y ser modificado
;Esta función permite que un usuario que haya iniciado secion pueda cerrar esta

(define logout (lambda (system)
                 (if(equal?(length(get-name system))3)
                    ;Reviso si es que el largo de la lista del get-name es igual a 3
                    (make-system(cons (list-ref(list-ref system 0)0)null)(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                    ;Si devuelve true entonces devuelvo toda la estructura del sistema dejando como unico elemento de la lista get-name el nombre del sistema
                    (if(equal? (length(get-name system))2)
                       ;Si no se cumple el if anterior entonces reviso si es que el largo de la lista del get-name es igual a 2
                       (make-system(cons (list-ref(list-ref system 0)0)null)(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                       ;Si devuelve true entonces devuelvo toda la estructura del sistema dejando como unico elemento de la lista get-name el nombre del sistema
                       (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))))))
                       ;Pero si devuelve false entonces devuelvo el sistema como estaba, ya que en tal caso no habria ninguna secion iniciada

;--------------------------------------------------------------------------------------------------

;Función Switch-drive
;Dominio: El sistema junto con el nombre de la unidad (drive) (tipo char) 
;Recorrido: Arroja el sistema luego de ejecutar la funcion y ser modificado
;Esta función permite fijar una unidad (drive) donde luego se crearan carpetas y archivos

(define switch-drive(lambda(system)(lambda(letter)
                                    (if(and(not(null?(list-ref(list-ref system 0)1)))(verificar(flatten(get-drive system))(car(string->list(string-upcase(string letter)))))(equal?(length(flatten(list-ref system 0)))2))
                                       ;Reviso si es que existe un usuario que inicio secion, tambien verifico si la letra de la unidad (drive) entregada existe en el get-drive (donde se almacenan las unidades existentes) y por ultimo reviso si el largo del get-name es igual a 2
                                       (make-system (flatten(cons(get-name system)(string-append(string-upcase(string letter))":/")))(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                                       ;Si se cumplen las tres condiciones entonces devuelvo toda la estructura del sistema añadiendole a la lista del get-name el elemento entregado por comando (tipo char) como tercer elemento de esta
                                       (if(not(verificar (get-drive system)(car(string->list(string-upcase(string letter))))))
                                          ;Si no se cumple el if anterior entonces ocupo el verificar negado entre el get-drive y la letra de la unidad (tipo char) entregada por comando
                                          (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                                          ;Si devuelve true entonces devuelvo el sistema como estaba, ya que no se puede iniciar una unidad que no este ingresada anteriormente
                                          (if(not(null?(list-ref(list-ref system 0)2)))
                                             ;Si no se cumple el if anterior entonces reviso si es que no esta vacio el tercer espacio de la lista del get-name
                                             (make-system (flatten(cons(cdr(cons(get-name system)(remove(list-ref(list-ref system 0)2)(get-name system))))(string-append(string-upcase(string letter))":/")))(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                                             ;Si devuelve true devuelvo toda la estructura del sistema dejando iniciado la unidad ingresada por comando, dejandola en la tercera posicion de la lista del get-name
                                             (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))))))))
                                             ;Si devuelve false entonces devuelvo el sistema como estaba, ya que no se pueden iniciar dos unidades al mismo tiempo

;--------------------------------------------------------------------------------------------------

;Función Md
;Dominio: El sistema junto con el nombre del directorio a crear (tipo string)
;Recorrido: Arroja el sistema luego de ejecutar la funcion y ser modificado
;Esta función permite crear un directorio (carpeta) dentro de la unidad, usuario y sistema iniciados 

(define md (lambda(system)(lambda(nam)
                            (if(null?(list-ref system 3))
                               ;Reviso si es que el get-carpetas esta vacio (donde se almacenan los directorios creados)
                               (make-system(get-name system)(get-drive system)(get-user system)(list(append(list(first(string->list(list-ref(list-ref system 0)2)))(string-append(list-ref(list-ref system 0)2)(string-downcase nam)"/") (list "null"))(list (fechayhora current-seconds) null (list-ref(list-ref system 0)1))))(get-papelera system)(get-fecha system))
                               ;Si devuelve true devuelvo toda la estructura del sistema añadiendole el directorio (especificando la unidad (drive), la ruta donde quedo, el contenido del archivo (si es que tiene), fecha y hora de creación y el usuario donde se creo
                               (if(verificar(flatten(list-ref system 3))(string-append(list-ref(list-ref system 0)2)(string-downcase nam)"/"))
                                  ;Si no se cumple el if anterior entonces realizo un verificar entre el get-carpetas y el nombre ingresado (tipo string) que esta unido a la ruta registrada en el tercer espacio de la lista del get-name y a un "/"
                                  (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                                  ;Si devuelve true entonces devuelvo el sistema como estaba, ya que no se pueden crear dos directorios iguales en una misma ruta
                                  (make-system(get-name system)(get-drive system)(get-user system)(append(list-ref system 3)(list(append(list(first(string->list(list-ref(list-ref system 0)2)))(string-append(list-ref(list-ref system 0)2)(string-downcase nam)"/")(list "null") )(list (fechayhora current-seconds) null (list-ref(list-ref system 0)1)))))(get-papelera system)(get-fecha system)))))))
                                  ;Si devuelve false devuelvo toda la estructura del sistema añadiendole el directorio (especificando la unidad (drive), la ruta donde quedo, el contenido del archivo (si es que tiene), fecha y hora de creación y el usuario donde se creo

;--------------------------------------------------------------------------------------------------

;Función Cd
;Dominio: El sistema junto con el nombre o la ruta del directorio creado (tipo string)
;Recorrido: Arroja el sistema luego de ejecutar la funcion y ser modificado
;Esta función permite cambiar la ruta en la que se esta trabajando ya sea ingresando a una carpeta de la direccion donde se encuentra o a una ruta especifica

(define cd (lambda(system)(lambda(d)
                            (if(equal?(last(string-split d ":")) "/")
                               (make-system(remplazar(list-ref system 0)2(string-upcase d))(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                               (if(equal? "/" d)
                               ;Reviso si el elemento ingresado por comando (tipo string) es igual a "/"
                               (make-system(flatten(cons(cdr(cons(get-name system)(remove(list-ref(list-ref system 0)2)(get-name system))))(string-append(string-join(take(string-split(list-ref(list-ref system 0)2)"/")1)"/")"/")))(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                               ;Si devuelve true devuelvo toda la estructura del sistema modificando el tercer elemento de la lista del get-name, dejando solo la letra de la unidad como ruta actual
                               (if(verificar-car(list-ref system 3)d)
                                  ;Si no se cumple el if anterior realizo un verificar-car entre el elemento ingresado por comando (tipo string) y los elementos de la lista del get-carpetas
                                  (make-system(flatten(cons(cdr(cons(get-name system)(remove(list-ref(list-ref system 0)2)(get-name system))))(string-append(car(string-split (string-upcase d) ":"))":"(cadr(string-split (string-downcase d) ":")))))(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                                  ;Si devuelve true devuelvo toda la estructura del sistema modificando el tercer elemento de la lista del get-name, dejando en el tercer espacio el elemento entregado por el comando (tipo string)
                                  (if(verificar-car(get-carpetas system)(string-append(list-ref(list-ref system 0)2)d"/"))
                                     ;Si no se cumple el if anterior realizo un verificar-car entre los elementos de la lista get-carpetas y el elemento entregado por comando (tipo estring) uniendo este con la ruta actual y un "/"
                                     (make-system(remplazar(list-ref system 0)2(string-append(list-ref(list-ref system 0)2)(string-downcase d)"/"))(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                                     ;Si devuelve true devuelvo toda la estructura del sistema remplazando el tercer elementos de la lista de get-name por el elemento entregado por comando (tipo string), este unido al anteiror tercer elemento de la lista de get-name
                                     (if(equal? ".." d)
                                        ;Si no se cumple el if anterior reviso si el elemento ingresado por comando (tipo string) es igual a ".."
                                        (make-system(flatten(cons(cdr(cons(get-name system)(remove(list-ref(list-ref system 0)2)(get-name system))))(string-append(string-join(take(string-split(list-ref(list-ref system 0)2)"/")(- (length(string-split(list-ref(list-ref system 0)2)"/"))1))"/")"/")))(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                                        ;Si devuelve true devuelvo toda la estructura del sistema modificando la ruta para que quite el ultimo elemento que este entremedio de "/", es decir retrocede una carpeta en la ruta actual
                                        (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))))))))))
                                        ;Si devuelve false entonces devuelvo el sistema como estaba, ya que no cumple con ninguna condición el comando proporcionado

;--------------------------------------------------------------------------------------------------

;Función Add-file
;Dominio: El sistema junto con un elemento tipo file que contiene el nombre del archivo, la extenmsión, el contenido de este y los atributos de seguridad (si es que tienen)
;Recorrido: Arroja el sistema luego de ejecutar la funcion y ser modificado
;Esta función añade un archivo a la ruta actual

(define add-file(lambda(system)(lambda(datos)
                                 ;devuelve el sistema añadiendo el archvio dentro de la ruta actual y deja de estar nulo el tercer elemento del directorio y pasa a estar el nombre, la extension, el conteido y los atributos de seguridad (si es que tienen) del archivo entregado por comando
                                 (make-system(get-name system)(get-drive system)(get-user system)(append (list-ref system 3)(list (append(list (first (string->list (list-ref(list-ref system 0)2)))(string-append(list-ref(list-ref system 0)2) (list-ref datos 0)))(list datos (fechayhora current-seconds) null (list-ref(list-ref system 0)1) null))))(get-papelera system)(get-fecha system)))))

;----------------------------------------------------------------------------------------------------

;Función Del
;Dominio: El sistema junto con el nombre o la ruta del directorio creado (tipo string)
;Recorrido: Arroja el sistema luego de ejecutar la funcion y ser modificado
;Esta función elimina uno o varios archivos de la ruta actual y estos se van a la papelera

(define del(lambda(system)(lambda (conte)
                            (if(equal? conte "*.*")
                               ;Reviso si el elemento ingresado por comando (tipo string) es igual a "*.*"
                               (borrar-archivo(filter(lambda(sublst)(ormap(lambda(x)(and(string? x)(regexp-match?(regexp(string-append(list-ref(list-ref system 0)2)))x)))sublst))(get-carpetas system))system)
                               ;Si devuelve true aplico la función borrar-archivo con los elementos de la ruta actual y el sistema
                               (if(verificar-ar (list-ref system 3)conte)
                                  ;Si no se cumple el if anterior realizo un verificar-ar entre el get-carpetas y el elemento entregado por comando (tipo string)
                                  (especifico(filter(lambda(sublst)(ormap(lambda(x)(and(string? x)(regexp-match?(regexp(string-append(list-ref(list-ref system 0)2)))x)))sublst))(get-carpetas system))(string-downcase conte)system)
                                  ;Si devuelve true aplico la función especifico con los elementos de la ruta actual, el elemento entregado por comando (tipo string) y el sistema 
                                  (if(and(equal?(car(string-split conte "."))"*")(member(string-downcase(last(string-split conte ".")))(flatten(list-ref system 3))))
                                     ;Si no se cumple el if anterior entonces reviso si el primer elemento o letra del elemento entegrado por comando (tipo string) es igual a "*" y ademas reviso si la extension de este elementose encuentra en get-carpetas
                                     (extension(filter(lambda(sublst)(ormap(lambda(x)(and(string? x)(regexp-match?(regexp(string-append(list-ref(list-ref system 0)2)))x)))sublst))(get-carpetas system))(string-downcase conte)system)
                                     ;Si devuelve true aplico la función extension con los elementos de la ruta actual, el elemento entregado por comando (tipo string) y el sistema 
                                     (if(verificar(flatten(list-ref system 3))(string-downcase(last(string-split conte "."))))
                                        ;Si no se cumple el if anterior realizo un verificar entre la extension del elemento entregado por comando (tipo string) y los elementos del get-carpetas
                                        (empieza-fin(filter(lambda(sublst)(ormap(lambda(x)(and(string? x)(regexp-match?(regexp(string-append (list-ref(list-ref system 0)2))) x))) sublst))(get-carpetas system))(string-downcase conte)system)
                                        ;Si devuelve true aplico la función empieza-fin con los elementos de la ruta actual, el elemento entregado por comando (tipo string) y el sistema 
                                        (if(and(lambda(x)(string-contains?(second x)(string-downcase conte)))(string-contains?(third(list-ref system 0))(string-downcase conte))) 
                                           ;Si no se cumple el if anterior entonces reviso si me ecuentro ubicado en el elemento ingresado por comando (tipo string)
                                           (make-system(flatten(cons(cdr(cons(get-name system)(remove(list-ref(list-ref system 0)2)(get-name system))))(string-append(string-join(take(string-split(list-ref(list-ref system 0)2)"/")(- (length(string-split(list-ref(list-ref system 0)2)"/"))1))"/")"/")))(get-drive system)(get-user system)(filter(lambda(x)(not(string-contains?(second x)(string-downcase conte))))(list-ref system 3))(filter(lambda(x)(not(member x (filter(lambda(x)(not(string-contains?(second x)(string-downcase conte))))(list-ref system 3)))))(list-ref system 3))(get-fecha system))
                                           ;Si devuelve true devuelvo toda la estructura del sistema modificando la ruta, devolviendome una carpeta atras, ademas de modificar el get-carpeta para eliminar todos los directorios que cumplan y mandarlos al get-papelera
                                           (if(lambda(x)(string-contains?(second x)(string-downcase conte)))
                                              ;Si no se cumple el if anterior entonces reviso si en la ruta actual esta el directorio que quiero eliminar
                                              (make-system(get-name system)(get-drive system)(get-user system)(filter(lambda(x)(not(string-contains?(second x)(string-downcase conte)))) (list-ref system 3))(filter (lambda (x) (not (member x  (filter (lambda (x) (not (string-contains? (second x) (string-downcase conte)))) (list-ref system 3)))))(list-ref system 3))(get-fecha system))
                                              ;Si devuelve true devuelvo toda la estructura del sistema modificando el get-carpeta para eliminar todos los directorios que cumplan y mandarlos al get-papelera
                                              (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system)))))))))))
                                              ;Si devuelve false entonces devuelvo el sistema como estaba, ya que no cumple con ninguna condición el comando proporcionado

;----------------------------------------------------------------------------------------------------

;Función Rd
;Dominio: El sistema junto con el nombre o la ruta del directorio creado (tipo string)
;Recorrido: Arroja el sistema luego de ejecutar la funcion y ser modificado
;Esta función elimina un directorio siempre y cuando este este vacio

(define rd(lambda(system)(lambda(carpeta)
                           (if(boolean?(member(string-downcase carpeta)(string-split(caddr(get-name system))"/")))
                              ;Reviso si es que se encuentra el elemento entregado por comando (tipo string) en la ruta actual
                              (if(verificar-rd(get-carpetas system)(string-downcase carpeta))
                                 ;Si devuelve true entonces aplico la función verificar-rd con el get-carpetas y el elemento ingresado por comando (tipo string)
                                 (make-system(get-name system)(get-drive system)(get-user system)(eliminar(get-carpetas system)(string-downcase carpeta))(append(get-papelera system)(añadir(get-carpetas system)(string-downcase carpeta)))(get-fecha system))
                                 ;Si devuelve true devuelvo toda la estructura del sistema modificando el get-capetas aplicandole la función eliminar y al get-papelera le aplico la función añadir 
                                 (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system)))
                                 ;Si devuelve false entonces devuelvo el sistema como estaba
                              (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))))))
                              ;Si devuelve false, ya que el elemento entregado por comando (tipo string) no esta vacio

;----------------------------------------------------------------------------------------------------

;Función Copy
;Dominio: El sistema junto con el archivo o carpeta de origen (tipo string) y la ruta de destino (tipo string)
;Recorrido: Arroja el sistema luego de ejecutar la funcion y ser modificado
;Esta función permite copiar una carpeta o un archivo de un lugar a una ruta especifica

(define copy(lambda(system)(lambda(nombre ruta)
                             (if(or(verificar(flatten(get-carpetas system))(string-append (string-append(car(string-split (string-upcase ruta) ":"))":"(cadr(string-split (string-downcase ruta) ":"))) (string-downcase nombre)))(verificar(flatten(get-carpetas system))(string-append (string-append(car(string-split (string-upcase ruta) ":"))":"(cadr(string-split (string-downcase ruta) ":")))(string-downcase nombre) "/")))
                                (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                                ;Si devuelve false entonces devuelvo el sistema como estaba
                                (if(and(verificar-car(get-carpetas system)(string-append(car(string-split (string-upcase ruta) ":"))":"(cadr(string-split (string-downcase ruta) ":"))))(member(string-downcase nombre)(flatten(get-carpetas system))))
                                   ;Realizo un verificar-car entre el get-carpetas y la ruta de destino donde se quiere copiar el elemento entregado por comando (tipo string) y ademas reviso si es que dentro del get-carpetas se encuentra ese elemento que se quiere copiar
                                   (make-system(get-name system)(get-drive system)(get-user system)(append(get-carpetas system)(arreglar(filter(lambda(sublst)(ormap(lambda(x)(and(string? x)(regexp-match?(regexp(string-append(list-ref(list-ref system 0)2)))x)))sublst))(get-carpetas system))(string-downcase nombre)(string-append(car(string-split(string-upcase ruta)":"))":"(cadr(string-split (string-downcase ruta) ":"))) system))(get-papelera system)(get-fecha system))
                                   ;Si devuelve true devuelvo toda la estructura del sistema agregando el elemento en su nueva ruta
                                   (if(and(not(null?(filter(lambda(sublst)(ormap(lambda(x)(and(string? x)(regexp-match?(regexp(string-downcase nombre))x)))sublst))(get-carpetas system))))(or(verificar-car(get-carpetas system)ruta)(member(first(string->list(car(string-split(string-upcase ruta)":"))))(get-drive system))))
                                      ;Si no se cumple el if anterior entonces realizo un verificar-car entre el get-carpetas y la ruta de destino donde se quiere copiar el elemento entregado por comando (tipo string) o reviso si es que dentro del get-drive se encuentra la unidad (drive) correspondiente a la ruta donde se quiere copiar el elemento y ademas reviso si no esta vacio un filtro que se le realiza al nombre del elemento que se quiere copiar
                                      (make-system(get-name system)(get-drive system)(get-user system)(append(get-carpetas system)(arreglar(filter(lambda(sublst)(ormap(lambda(x)(and(string? x)(regexp-match?(regexp(string-append(list-ref(list-ref system 0)2)))x)))sublst))(get-carpetas system))(string-downcase nombre)(string-append(car(string-split(string-upcase ruta)":"))":"(cadr(string-split (string-downcase ruta) ":"))) system))(get-papelera system)(get-fecha system))
                                      ;Si devuelve true devuelvo toda la estructura del sistema agregando el elemento en su nueva ruta
                                      (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))))))))
                                      ;Si devuelve false entonces devuelvo el sistema como estaba

;----------------------------------------------------------------------------------------------------

;Función Move
;Dominio: El sistema junto con el archivo o carpeta de origen (tipo string) y la ruta de destino (tipo string)
;Recorrido: Arroja el sistema luego de ejecutar la funcion y ser modificado
;Esta función permite mover una carpeta o un archivo de un lugar a una ruta especifica

(define move(lambda(system)(lambda (nombre ruta)
                             (if(or(verificar(flatten(get-carpetas system))(string-append (string-append(car(string-split (string-upcase ruta) ":"))":"(cadr(string-split (string-downcase ruta) ":"))) (string-downcase nombre)))(verificar(flatten(get-carpetas system))(string-append (string-append(car(string-split (string-upcase ruta) ":"))":"(cadr(string-split (string-downcase ruta) ":")))(string-downcase nombre) "/")))
                                (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                                ;Si devuelve false entonces devuelvo el sistema como estaba
                                (if(and(member ruta(flatten(get-carpetas system)))(member(string-downcase nombre)(flatten(get-carpetas system))))
                                   ;Reviso si la ruta a la que se quiere mover el elemento (tipo string) esta dentro del get-carpetas y ademas reviso si el nombre del elemento esta dentro del get-carpetas
                                   (make-system(get-name system)(get-drive system)(get-user system)(append(eli-move(filter(lambda(sublst)(ormap(lambda(x)(and(string? x)(regexp-match? (regexp (string-append (list-ref(list-ref system 0)2))) x)))sublst))(get-carpetas system))(string-downcase nombre) (string-append(car(string-split (string-upcase ruta) ":"))":"(cadr(string-split (string-downcase ruta) ":"))) system)(filter (lambda (sublst)(not(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (string-append (list-ref(list-ref system 0)2))) x))) sublst)))(get-carpetas system))(arreglar(filter (lambda (sublst)(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (string-append (list-ref(list-ref system 0)2))) x))) sublst))(get-carpetas system)) (string-downcase nombre) (string-append(car(string-split (string-upcase ruta) ":"))":"(cadr(string-split (string-downcase ruta) ":"))) system))(get-papelera system)(get-fecha system))
                                   ;Si devuelve true devuelvo toda la estructura del sistema agregando el elemento en su nueva ruta y eliminandolo de la ruta anterior
                                   (if(and(not(null?(filter(lambda(sublst)(ormap(lambda(x)(and(string? x)(regexp-match?(regexp(string-downcase nombre))x)))sublst))(get-carpetas system))))(or(verificar-car(get-carpetas system)ruta)(member(first(string->list (car(string-split(string-upcase ruta)":"))))(get-drive system))))
                                      ;Si no se cumple el if anterior entonces realizo un verificar-car entre el get-carpetas y la ruta de destino donde se quiere copiar el elemento entregado por comando (tipo string) o reviso si es que dentro del get-drive se encuentra la unidad (drive) correspondiente a la ruta donde se quiere copiar el elemento y ademas reviso si no esta vacio un filtro que se le realiza al nombre del elemento que se quiere copiar
                                      (make-system(get-name system)(get-drive system)(get-user system)(append(eli-move(filter(lambda(sublst)(ormap(lambda(x)(and(string? x)(regexp-match? (regexp (string-append (list-ref(list-ref system 0)2))) x)))sublst))(get-carpetas system))(string-downcase nombre)(string-append(car(string-split (string-upcase ruta) ":"))":"(cadr(string-split (string-downcase ruta) ":"))) system)(filter (lambda (sublst)(not(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (string-append (list-ref(list-ref system 0)2))) x))) sublst)))(get-carpetas system))(arreglar (filter (lambda (sublst)(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (string-append (list-ref(list-ref system 0)2))) x))) sublst))(get-carpetas system)) (string-downcase nombre) (string-append(car(string-split (string-upcase ruta) ":"))":"(cadr(string-split (string-downcase ruta) ":"))) system))(get-papelera system)(get-fecha system))
                                      ;Si devuelve true devuelvo toda la estructura del sistema agregando el elemento en su nueva ruta y eliminandolo de la ruta anterior
                                      (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))))))))
                                      ;Si devuelve false entonces devuelvo el sistema como estaba
;----------------------------------------------------------------------------------------------------

;Función Ren
;Dominio: El sistema junto con el nombre actual del archivo o carpeta (tipo string) y el nombre nuevo (tipo string)
;Recorrido: Arroja el sistema luego de ejecutar la funcion y ser modificado
;Esta función permite cambiar el nombre de una carpeta o un archivo 

(define ren(lambda(system)(lambda(original nuevo)
                            (if(verificar-ren(filter(lambda(sublst)(ormap(lambda(x)(and(string? x)(regexp-match?(regexp(list-ref(list-ref system 0)2)) x)))sublst))(get-carpetas system))(string-downcase nuevo) system)
                               ;Realizo un verificar-ren con el sistema, el nombre original del elemento y un filtro aplicado a la ruta actual del sistema
                               (if(verificar(flatten(get-carpetas system))(string-downcase original))
                                  ;Realizo un verificar entre el get-carpetas y el nombre original del elemento ingresado por comando (tipo string)
                                  (make-system(get-name system)(get-drive system)(get-user system)(append(filter(lambda(sublst)(not(ormap(lambda (x)(and (string? x)(regexp-match?(regexp(list-ref(list-ref system 0)2))x)))sublst)))(get-carpetas system))(cambiont (filter (lambda (sublst)(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (string-append (list-ref(list-ref system 0)2))) x)))sublst))(get-carpetas system))(string-downcase original)(string-downcase nuevo)system)(cambio (filter (lambda (sublst)(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (string-append (list-ref(list-ref system 0)2))) x)))sublst))(get-carpetas system))(string-downcase original)(string-downcase nuevo)system))(get-papelera system)(get-fecha system))
                                  ;Si devuelve true devuelvo toda la estructura del sistema modificando el nombre de la carpeta 
                                  (if(not(null?(filter(lambda(sublst)(ormap(lambda(x)(and(string? x)(regexp-match?(regexp(string-downcase original)) x)))sublst))(get-carpetas system))))
                                     ;reviso si no esta vacio un filtro que se le realiza al nombre original del elemento que se quiere copiar
                                     (make-system(get-name system)(get-drive system)(get-user system)(append(filter(lambda(sublst)(not(ormap (lambda (x)(and (string? x)(regexp-match?(regexp(list-ref(list-ref system 0)2))x)))sublst)))(get-carpetas system))(cambiont2 (filter (lambda (sublst)(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (string-append (list-ref(list-ref system 0)2))) x)))sublst))(get-carpetas system))(string-downcase original)(string-downcase nuevo)system)(cambio2 (filter (lambda (sublst)(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (string-append (list-ref(list-ref system 0)2))) x)))sublst))(get-carpetas system))(string-downcase original)(string-downcase nuevo)system))(get-papelera system)(get-fecha system))
                                     ;Si devuelve true devuelvo toda la estructura del sistema modificando el nombre del archivo
                                     (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))))
                                     ;Si devuelve false entonces devuelvo el sistema como estaba
                               (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))))))
                               ;Si devuelve false entonces devuelvo el sistema como estaba

;----------------------------------------------------------------------------------------------------

;Función Dir
;Dominio: El sistema junto con un elemento (tipo string), el cual termina siendo un elelemnto opcional
;Recorrido: Entrega un string con el resultado
;Esta función muestra el contenido de un directorio específico o de toda una ruta

(define dir (lambda(system)(lambda para
 (if(null?  para)
    ;Reviso si el elemento entregado por comando es nulo
    (for-each my-display(dir-ruta-h (filter (lambda (sublst)(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (list-ref(list-ref system 0)2))x)))sublst))(get-carpetas system)) (list-ref(list-ref system 0)2)))
    ;Si el elemento entregado por comando es nulo se ejecuta la función dir-ruta-h para mostrar el string resultante
  (cond                                            
    [(string-ci=? (car para) "/s")(for-each my-display(dir-ruta-t-h (filter (lambda (sublst)(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (list-ref(list-ref system 0)2)) x)))sublst))(get-carpetas system)) (list-ref(list-ref system 0)2)))]
    ;Si el elemento entregado por comando es igual a "/s" se ejecuta la función dir-ruta-t-h para mostrar el string resultante
    [(string-ci=? (car para) "/a")(for-each my-display(dir-ruta (filter (lambda (sublst)(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (list-ref(list-ref system 0)2)) x)))sublst))(get-carpetas system)) (list-ref(list-ref system 0)2)))]
    ;Si el elemento entregado por comando es igual a "/a" se ejecuta la función dir-ruta para mostrar el string resultante
    [(string-ci=? (car para) "/s /a")(for-each my-display(dir-ruta-t (filter (lambda (sublst)(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (list-ref(list-ref system 0)2)) x)))sublst))(get-carpetas system)) (list-ref(list-ref system 0)2)))]
    ;Si el elemento entregado por comando es igual a "/s /a" se ejecuta la función dir-ruta-t para mostrar el string resultante
    [(string-ci=? (car para) "/o N")(for-each my-display(ordenar-alfa (dir-ruta-h (filter (lambda (sublst)(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (list-ref(list-ref system 0)2)) x)))sublst))(get-carpetas system)) (list-ref(list-ref system 0)2))))]
    ;Si el elemento entregado por comando es igual a "/o N" se ejecuta la función dir-ruta-h para mostrar el string resultante
    [(string-ci=? (car para) "/o D")(for-each my-display(nume-asc (dir-ruta-h (filter (lambda (sublst)(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (list-ref(list-ref system 0)2)) x)))sublst))(get-carpetas system)) (list-ref(list-ref system 0)2))))]
    ;Si el elemento entregado por comando es igual a "/o D" se ejecuta la función dir-ruta-h para mostrar el string resultante
    [(string-ci=? (car para) "/o -D")(for-each my-display(reverse(nume-asc (dir-ruta-h (filter (lambda (sublst)(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (list-ref(list-ref system 0)2)) x)))sublst))(get-carpetas system)) (list-ref(list-ref system 0)2)))))]
    ;Si el elemento entregado por comando es igual a "/o -D" se ejecuta la función dir-ruta-h junto con el reverse para mostrar el string resultante dela función e invertirlo
    [(string-ci=? (car para) "/?")(string-append 
"Si no se ingresa nada solo se mostra el contenido del directorio actual, excluyendo subdirectorios y contenido oculto
Si ingresa '/s' se mostrara el contenido del directorio actual junto a subdirectorios, pero excluyendo el contenido oculto.
Si ingresa '/a' se mostrara solo el contenido del directorio actual incluyendo el contenido oculto.
Si ingresa '/s' /a se mostra el contenido del directorio actual y subdirectorios, incluyendo contenido oculto.
Si ingresa '/N' se mostrara el contenido del directorio actual en orden alfabético ascendente.
Si ingresa '/o D' se mostrara el contenido del directorio actual según fecha de creación, en orden ascendente. 
Si ingresa '/o D' se mostrara el contenido del directorio actual según fecha de creación, en orden descendente
Si ingresa cualquier otra cosa se mostrara 'condición inválida'")]
    ;Si el elemento entregado por comando es igual a "/?" se muestra el string resultante
    [else(display "condición inválida" )])))))
    ;Si el elemento entregado por comando no es igual a ninguna de los string se muestra el string resultante

;----------------------------------------------------------------------------------------------------

;Función Formart
;Dominio: El sistema junto con la letra de la unidad (drive) (tipo char) y el nuevo nombre de esta (tipo string)
;Recorrido: Arroja el sistema luego de ejecutar la funcion y ser modificado
;Esta función permite borrar todos los elementos de una unidad (drive) y cambiarle el nombre a esta unidad

(define format(lambda(system)(lambda(letter nombre)
                               (if(verificar(flatten(get-drive system))(car(string->list(string-upcase(string letter)))))
                                  ;Reviso con un verificar si la letra de la unidad (drive) se encuentra en el get-drive
                                  (if(member letter(get-name system))
                                     ;Si es asi, reviso si la letra de la unidad se encuentra en el get-name
                                     (make-system(flatten(cons(cdr(cons(get-name system)(remove(list-ref(list-ref system 0)2)(get-name system))))(string-append(string-join(take(string-split(list-ref(list-ref system 0)2)"/")1)"/")"/")))(remplazar(flatten(list-ref system 1))(+(index-of (flatten(list-ref system 1))(findf (lambda (x) (equal? x (car(string->list(string-upcase(string letter))))))(flatten(list-ref system 1))))1) (string-upcase nombre))(get-user system)(filter (lambda (sublista) (not (member (car(string->list(string-upcase(string letter)))) sublista)))(list-ref system 3))(get-papelera system)(get-fecha system))
                                     ;Si es asi entonces coloco como ruta la letra de la unidad (drive), cambio su nombre y borro todos los elementos que esta tenia
                                     (make-system(get-name system)(remplazar(flatten(list-ref system 1))(+(index-of (flatten(list-ref system 1))(findf (lambda (x) (equal? x (car(string->list(string-upcase(string letter))))))(flatten(list-ref system 1))))1) (string-upcase nombre))(get-user system)(filter (lambda (sublista) (not (member (car(string->list(string-upcase(string letter)))) sublista)))(list-ref system 3))(get-papelera system)(get-fecha system))) 
                                     ;Si no es asi, entonces cambio el nombre de la unidad (drive) y borro todos los elementos que esta tenia
                                  (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))))))
                                  ;Si no es asi entonces devuelvo el sistema como estaba, ya que no se puede formatear una unidad que no esta ingresada

;----------------------------------------------------------------------------------------------------

;Función Encrypt
;Dominio: El sistema junto con el metodo para encryptar y desencriptar el archivo o carpeta, ademas de la contraseña (tipo string) y el elemento a encriptar (tipo string)
;Recorrido: Arroja el sistema luego de ejecutar la funcion y ser modificado
;Esta función permite encriptar un archivo o carpeta y todo su contenido

(define encrypt(lambda (system)(lambda (encry decry contra elemento)
                                 (if(verificar (flatten (filter (lambda (sublst)(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (list-ref(list-ref system 0)2)) x)))sublst))(get-carpetas system))) (string-downcase elemento))
                                    ;Reviso con un verificar si el elemento a encriptar esta en la ruta actual
                                    (make-system(get-name system)(get-drive system)(get-user system)(append (filter (lambda (sublst)(not(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (list-ref(list-ref system 0)2)) x)))sublst)))(get-carpetas system))(eli-encr-ar  (filter (lambda (sublst)(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (list-ref(list-ref system 0)2)) x)))sublst))(get-carpetas system)) (string-downcase elemento) system)(encriptar-ar  (filter (lambda (sublst)(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (list-ref(list-ref system 0)2)) x)))sublst))(get-carpetas system)) encry decry contra (string-downcase elemento) system))(get-papelera system)(get-fecha system))
                                    ;Si es asi, encripto el archivo con la función encriptar-ar y luego elimino la original con eli-encr-ar
                                    (make-system(get-name system)(get-drive system)(get-user system)(append (filter (lambda (sublst)(not(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (list-ref(list-ref system 0)2)) x)))sublst)))(get-carpetas system))(filter (lambda (sublst)(not(ormap (lambda (x)(and (string? x)(regexp-match? (regexp(string-downcase elemento)) x)))sublst)))(filter (lambda (sublst)(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (list-ref(list-ref system 0)2)) x)))sublst))(get-carpetas system)))(eli-encr-car  (filter (lambda (sublst)(ormap (lambda (x)(and (string? x) (regexp-match? (regexp (string-downcase elemento)) x)))sublst))(filter (lambda (sublst)(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (list-ref(list-ref system 0)2)) x)))sublst))(get-carpetas system)))(string-downcase elemento) system)(encriptar-car  (filter (lambda (sublst)(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (string-downcase elemento)) x)))sublst))(filter (lambda (sublst)(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (list-ref(list-ref system 0)2)) x)))sublst))(get-carpetas system))) encry decry contra (string-downcase elemento) system))(get-papelera system)(get-fecha system))))))
                                    ;Si no es asi, encripto los elementos del directorio con la función encriptar-car y luego elimino los originales con eli-encr-car

;----------------------------------------------------------------------------------------------------

;Función Decrypt
;Dominio: El sistema junto con la contraseña (tipo string) y el elemento a desencriptar (tipo string)
;Recorrido: Arroja el sistema luego de ejecutar la funcion y ser modificado
;Esta función permite desencriptar un archivo o carpeta y todo su contenido

(define decrypt(lambda (system) (lambda (contra elemento)
                                  (if(verificar (flatten (filter (lambda (sublst)(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (list-ref(list-ref system 0)2)) x)))sublst))(get-carpetas system))) (string-downcase elemento))
                                     ;Reviso con un verificar si el elemento a desencriptar esta en la ruta actual
                                     (make-system(get-name system)(get-drive system)(get-user system)(append (filter (lambda (sublst)(not(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (list-ref(list-ref system 0)2)) x)))sublst)))(get-carpetas system))(eli-decry-ar  (filter (lambda (sublst)(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (list-ref(list-ref system 0)2)) x)))sublst))(get-carpetas system)) (string-downcase elemento) system)(decry-ar (filter (lambda (sublst)(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (list-ref(list-ref system 0)2)) x)))sublst))(get-carpetas system)) contra (string-downcase elemento) system))(get-papelera system)(get-fecha system))
                                     ;Si es asi, desencripto el archivo con la función decry-ar y luego elimino la original con eli-decry-ar
                                     (make-system(get-name system)(get-drive system)(get-user system)(append (filter (lambda (sublst)(not(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (list-ref(list-ref system 0)2)) x)))sublst)))(get-carpetas system))(filter (lambda (sublst)(not(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (string-downcase elemento)) x)))sublst)))(filter (lambda (sublst)(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (list-ref(list-ref system 0)2)) x)))sublst))(get-carpetas system)))(eli-decry-car(filter (lambda(sublst)(ormap(lambda(x)(and (string? x)(regexp-match? (regexp(string-downcase elemento)) x)))sublst))(filter (lambda (sublst)(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (list-ref(list-ref system 0)2)) x)))sublst))(get-carpetas system))) (string-downcase elemento) system)(decry-car (filter (lambda (sublst)(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (string-downcase elemento)) x)))sublst))(filter (lambda (sublst)(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (list-ref(list-ref system 0)2)) x)))sublst))(get-carpetas system))) contra (string-downcase elemento) system))(get-papelera system)(get-fecha system))))))
                                     ;Si no es asi, desencripto los elementos del directorio con la función decry-car y luego elimino los originales con eli-decry-car

;----------------------------------------------------------------------------------------------------

;Función Plus-one
;Dominio: El string al que se le quiere aplicar la suma del codigo ASCII
;Recorrido: El string resultante
;Esta función transforma un String sumando al código ASCII de cada carácter un 1

(define plus-one(lambda(palabra)
                  (list->string (ascii+(string->list palabra)))))

;----------------------------------------------------------------------------------------------------

;Función Minus-one
;Dominio: El string al que se le quiere aplicar la resta del codigo ASCII
;Recorrido: El string resultante
;Esta función transforma un String restando al código ASCII de cada carácter un 1

(define minus-one(lambda (palabra)
                   (list->string (ascii-(string->list palabra)))))

;----------------------------------------------------------------------------------------------------

;Función Grep
;Dominio: El sistema junto un string y el nombre del archivo a ver o la ruta de este (tipo string)
;Recorrido: Entrega un string con el resultado
;Esta función permite buscar dentro del contenido de un archivo específico o dentro de una ruta

(define grep(lambda(system)(lambda(palabra ubicacion)
                             (if(verificar(flatten(filter(lambda(sublst)(ormap(lambda(x)(and(string? x)(regexp-match?(regexp(list-ref(list-ref system 0)2)) x)))sublst))(get-carpetas system))) (string-downcase ubicacion))
                                ;Reviso con un verificar si es que la ruta de la palabra esta dentro de la ruta actual
                                (grep-ayuda-ar(filter(lambda(sublst)(ormap(lambda(x)(and(string? x)(regexp-match?(regexp(string-downcase ubicacion)) x)))sublst))(filter (lambda (sublst)(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (list-ref(list-ref system 0)2)) x)))sublst))(get-carpetas system))) palabra system)
                                ;Si es asi con ayuda del grep-ayuda-ar muestro el string resultante
                                (grep-ayuda-car(filter(lambda(sublst)(ormap(lambda(x)(and(string? x)(regexp-match?(regexp(list-ref(list-ref system 0)2)) x)))sublst))(get-carpetas system)) palabra  system)))))
                                ;Si no es asi con ayuda del grep-ayuda-car muestro el string resultante

;----------------------------------------------------------------------------------------------------

;Función ViewTrash
;Dominio: El sistema 
;Recorrido: Entrega un string con el resultado
;Esta función permite obtener el contenido de la papelera de reciclaje de un sistema

(define viewTrash(lambda (system)
                   (newline)
                   (for-each my-display (get-papelera system))))

;----------------------------------------------------------------------------------------------------

;Función Restore
;Dominio: El sistema junto un string o el nombre del archivo o de una carpeta (tipo string)
;Recorrido: Arroja el sistema luego de ejecutar la funcion y ser modificado
;Esta función permite restaurar contenido específico dentro de la papelera para ubicarlo en su ubicación original

(define restore(lambda(system)(lambda(condicion)
                                (if(equal? condicion "*.*")
                                   ;Reviso si el string entrantre es igual a "*.*"
                                   (make-system(get-name system)(get-drive system)(get-user system)(append (get-carpetas system)(agregar-todo (get-papelera system)))null(get-fecha system))
                                   ;Si es asi con ayuda de la función agregar-todo restauro todos los elementos de la papelera 
                                   (if(verificar(flatten(get-carpetas system))(string-downcase condicion))
                                      ;Si no es asi, revuso si el string entrante se encuentra en el get-carpetas
                                      (make-system(get-name system)(get-drive system)(get-user system)(append (get-carpetas system)(agregar-ar (get-papelera system)(string-downcase condicion)))(eliminar-ar (get-papelera)(string-downcase condicion))(get-fecha system))
                                      ;Si devuelve true ocupo el agrgegar-ar para restaurar los archivos que coinsidan con el string y luego ocupo el eliminar-ar para borrar estos de la papelera
                                      (if(not(null? (filter (lambda (sublst)(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (string-downcase condicion)) x)))sublst))(get-papelera system))))
                                         ;Si devuevle false reviso el filtro resultante al ver si el string se encuentra en la papalera no es nulo
                                         (make-system (get-name system)(get-drive system)(get-user system)(append (get-carpetas system)(agregar-car condicion (filter (lambda (sublst)(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (string-downcase condicion)) x)))sublst))(get-papelera system))))(eliminar-car(get-papelera system))(get-fecha system))
                                         ;Si no es nulo, ocupo el agrgegar-car para restaurar los elementos que coinsidan con el string y luego ocupo el eliminar-car para borrar estos de la papelera
                                         (make-system (get-name system)(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))))))))
                                         ;Si es nulo devuelvo el sistema como estaba, ya que el elemento a restaurar no existe


