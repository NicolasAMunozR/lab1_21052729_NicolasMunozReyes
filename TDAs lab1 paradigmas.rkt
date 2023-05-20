#lang racket

(provide (all-defined-out))

;Funcion para colocar la fecha en que hago las modificaciones, especificando año-mes-dia hora-minutos-segundos
(define fechayhora(lambda (time)
                    (string-append (number->string(date-year(seconds->date(time))))"-"(number->string(date-month(seconds->date(time))))"-"(number->string(date-day(seconds->date(time))))" "(number->string(date-hour(seconds->date(time))))":"(number->string(date-minute(seconds->date(time))))":"(number->string(date-second(seconds->date(time)))))))

;El make-system sirve para dar la estructura al sistema
(define (make-system name drive user carpetas papelera fecha)(list name drive user carpetas papelera fecha))

;defino la posicion de la estructura que tendra el sistema
(define get-name car)

(define get-drive cadr)

(define get-user caddr)

(define get-carpetas cadddr)

(define get-papelera fifth)

(define get-fecha sixth)

;El make-drive sirve para dar la estructura al add-drive
(define (make-drive letra nombre capacidad)(list (string->list(string-upcase(string letra)))(string-upcase nombre)capacidad))

;Función para verificar si un elemento se encuentra dentro de una lista
(define verificar(lambda (disco ruta)
    (if (member ruta disco)
        #t
        #f)))

;Funcion para verificar si un elemento esta en una lista, con la excepcion que retorna true si es que son iguales pero no tienen las mismas mayusculas y minusculas (ejemplo "Hola" y "holA" arrojaria ture)
(define verificar-str(lambda(lista elemento)
                       (if(null? lista)
                          #f
                          (if(string-ci=? elemento(car lista))
                             #t
                             (verificar-str(cdr lista)elemento)))))

;Función igual que el verificar-str pero que funciona para ver especificamente el segundo elemento de cada direcorio creado 
(define verificar-car(lambda (lista elemento)
                       (if(null? lista)
                          #f
                          (if(string-ci=? elemento(cadr(car lista)))
                             #t
                             (verificar-car(cdr lista)elemento)))))

;Función que sirve para remplazar un elemento por uno nuevo entregando los dos elementos y la posición en la que se encuentra el elemento original
(define (remplazar lista posicion nombre-nuevo)
  (map (lambda (elemento i)
         (if (= i posicion) nombre-nuevo elemento))
       lista 
       (range 0 (length lista))))


;Esta función define los elementos entregados por comando en el add-file como un file donde entran los elementos obligatorios y opcionales del archivo
(define file(lambda(nombre tipo contenido . caracteres)
              (list (string-downcase nombre)(string-downcase tipo)contenido caracteres)))

;Función que ayuda a borrar todos los archivos cuando me entreguen su extension especifica
(define extension(lambda (disco conte system)
                   (if(null? disco)
                      (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                      (make-system(get-name system)(get-drive system)(get-user system)(append (filter (lambda (sublst)(not (ormap (lambda (x)(and (string? x)(regexp-match? (regexp (string-append  (list-ref(list-ref system 0)2))) x))) sublst)))(get-carpetas system))(filter (lambda (sublst)(not (ormap (lambda (x)(and (string? x)(regexp-match? (regexp (string-append "."conte"$")) x))) sublst)))disco))(filter (lambda (sublst)(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (string-append "."conte"$")) x))) sublst))disco)(get-fecha system)))))

;Función que ayuda a borrar todos los archivos cuando me entrguen su letra inicial y la extension con la que termina
(define empieza-fin(lambda (disco conte system)
                     (if(null? disco)
                        (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                        (make-system(get-name system)(get-drive system)(get-user system)(append (filter (lambda (sublst)(not(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (string-append (list-ref(list-ref system 0)2))) x))) sublst)))(get-carpetas system))(filter (lambda (sublst)(not(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (last (string-split conte "*"))) (car(caddr sublst)))(regexp-match? (regexp (first (string-split conte "*"))) (car(caddr sublst))))) sublst)))disco))(filter (lambda (sublst)(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (last (string-split conte "*"))) (car(caddr sublst)))(regexp-match? (regexp (first (string-split conte "*"))) (car(caddr sublst))))) sublst))disco)(get-fecha system)))))

;Función que ayuda a borrar un archivo especifco
(define especifico(lambda (disco conte system)
                    (if(null? disco)
                       (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                       (make-system(get-name system)(get-drive system)(get-user system)(append(filter(lambda(sublst)(not(ormap(lambda(x)(and(string? x)(regexp-match? (regexp (string-append (list-ref(list-ref system 0)2))) x))) sublst)))(get-carpetas system))(foldr (lambda (e acc)(if(and (list? e) (member conte (flatten e)) (member (string-append (list-ref(list-ref system 0)2) conte) (flatten e)))acc(cons e acc) ))'()disco))(foldr (lambda (e acc)(if (and (list? e) (member conte (flatten e)) (member (string-append (list-ref(list-ref system 0)2) conte) (flatten e)))(cons e acc)acc))'()disco)(get-fecha system)))))

;Función que ayuda a borrar todos los archivos de la ruta actual
(define borrar-archivo(lambda (disco system)
                        (if(null? disco)
                           (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                           (make-system(get-name system)(get-drive system)(get-user system)(append (filter (lambda (sublst)(not(ormap (lambda (x)(and (string? x)(regexp-match? (regexp (string-append (list-ref(list-ref system 0)2))) x))) sublst)))(get-carpetas system))(filter (and(lambda (x) (< (length x) 7)))disco))(filter (and(lambda (x) (>= (length x) 7)))disco)(get-fecha system)))))

;Función parecida a verificar-str pero se realiza entre el elemento y el primer elemento del tercer elemento de la lista  de los directorios del get-carpetas
(define verificar-ar(lambda (lista elemento)
                      (if(null? lista)
                         #f
                         (if(string-ci=? elemento (car(caddr(car lista))))
                            #t
                            (verificar-ar (cdr lista) elemento)))))

;Función que ayuda a revisar si el directorio que se quiere borrar se encuentra vacio
(define verificar-rd(lambda (disco carpeta)
                      (if(null? disco)
                         #t
                         (if(list?(member carpeta(string-split(cadr(car disco))"/")))  
                             (if(=(length (car disco))7)
                                #f
                                (verificar-rd(cdr disco)carpeta))
                             (verificar-rd(cdr disco)carpeta)))))

;Función que ayuda a eliminar el directorio que se quiere eliminar
(define eliminar(lambda(disco elemento)
                  (if(null? disco)
                     null
                     (if(or(list?(member elemento (string-split(cadr(car disco)) "/")))(member elemento (car disco)))  
                        (eliminar (cdr disco) elemento)
                        (cons (car disco) (eliminar (cdr disco) elemento))))))

;Función que ayuda a añadir a la papelera el directorio que se quiere borrar
(define añadir(lambda(disco elemento)
                (if(null? disco)
                   null
                   (if(or(list? (member elemento (string-split(cadr(car disco)) "/")))(member elemento (car disco)))  
                      (cons (car disco)(añadir (cdr disco) elemento))
                      (añadir(cdr disco)elemento)))))

;Función que ayuda a copiar el elemento que se se quiere copiar en una ruta especifica
(define arreglar(lambda(disco nombre ruta system)
                  (if(null? disco)
                     null
                     (if(equal? (length(car disco))6)
                        (if(member nombre (string-split(cadr(car disco))"/"))
                           (cons (list (car (string->list ruta))(string-append ruta(string-join(member nombre(string-split(cadr(car disco))"/"))"/")"/")(caddr(car disco))(fechayhora current-seconds)null (list-ref(list-ref system 0)1))(arreglar (cdr disco) nombre ruta system))
                           (arreglar (cdr disco) nombre ruta system))
                        (if(member nombre (string-split(cadr(car disco))"/"))
                           (cons (list (car (string->list ruta))(string-append ruta(string-join(member nombre(string-split(cadr(car disco))"/"))"/"))(caddr(car disco))(fechayhora current-seconds)null (list-ref(list-ref system 0)1)null)(arreglar (cdr disco) nombre ruta system))
                           (arreglar (cdr disco) nombre ruta system))))))

;Función que ayuda eliminar los elementos originales, ya que ahora estan en otra ruta, puesto que se movieron
(define eli-move(lambda(disco nombre ruta system)
                  (if(null? disco)
                     (make-system(get-name system)(get-drive system)(get-user system)(get-carpetas system)(get-papelera system)(get-fecha system))
                     (filter(lambda(sublst)(not(ormap(lambda(x)(and(string? x)(regexp-match? (regexp nombre)x)))sublst)))disco))))

;Función que ayuda a verificar si el nuevo nombre que se quiere colocar ya esta en el get-carpetas
(define verificar-ren(lambda (disco nuv system)
                 (if(null? disco)
                    #f
                    (if(verificar (flatten disco) nuv)
                       #f
                       #t))))

;Función que me devuelve una nueva lista hasta cierto elemento
(define (lista-hasta lista elem)
  (if (null? lista)
      null
      (if (equal? (car lista) elem)
          null
          (cons (car lista) (lista-hasta (cdr lista) elem)))))

;Función que ayuda a cambiar el nombre de las carpetas
(define cambio2(lambda(disco ori nuv system)
                 (if(null? disco)
                    null
                    (if(member ori (string-split(cadr(car disco)) "/"))
                       (if(and(not(null?(filter (lambda (sublst)(ormap (lambda (x)(and (string? x)(regexp-match? (regexp ori) x)))sublst)) disco)))(equal? (length (car disco)) 6))
                          (cons(list (car(car disco))(string-append (string-join(lista-hasta (string-split(cadr(car disco)) "/") ori)"/")"/"(string-join(remplazar (member ori(string-split(cadr(car disco)) "/")) 0 nuv)"/")"/")(caddr(car disco))(fechayhora current-seconds) null (list-ref(list-ref system 0)1) )(cambio2 (cdr disco) ori nuv system))
                          (cons(list (car(car disco))(string-append (string-join(lista-hasta (string-split(cadr(car disco)) "/") ori)"/")"/"(string-join(remplazar (member ori(string-split(cadr(car disco)) "/")) 0 nuv)"/"))(caddr(car disco))(fechayhora current-seconds) null (list-ref(list-ref system 0)1) null )(cambio2 (cdr disco) ori nuv system)))
                       (cambio2 (cdr disco) ori nuv system)))))

;Función que ayuda a mantener el nombre de las carpetas
(define cambiont2(lambda(disco ori nuv system)
                 (if(null? disco)
                    null
                    (if(member ori (string-split(cadr(car disco)) "/"))
                       (cambiont2 (cdr disco) ori nuv system)
                       (cons (car disco)(cambiont2 (cdr disco) ori nuv system))))))

;Función que ayuda a cambiar el nombre de los archivos
(define cambio(lambda (disco ori nuv system)
                (if(null? disco)
                   null
                   (if(and(not(verificar (flatten disco) nuv))(verificar (flatten (car disco)) ori))
                      (cons (list (car(car disco))(string-join(remplazar (string-split(cadr(car disco)) "/")(- (length(string-split(cadr(car disco)) "/"))1) nuv)"/")(remplazar (caddr(car disco)) 0 nuv)(fechayhora current-seconds) null (list-ref(list-ref system 0)1) null )(cambio (cdr disco) ori nuv system))
                      (cambio (cdr disco) ori nuv system)))))

;Función que ayuda a mantener el nombre de los archivos
(define cambiont(lambda (disco ori nuv system)
                (if(null? disco)
                   null
                   (if(and(not(verificar (flatten disco) nuv))(verificar (flatten (car disco)) ori))
                      (cambiont (cdr disco) ori nuv system)
                      (cons (car disco)(cambiont (cdr disco) ori nuv system))))))

;Función que ayuda a mostrar la ruta actual sin subcarpetas
(define dir-ruta(lambda (disco ruta)
                  (if(null? disco)
                     null
                     (if(eq?(length(string-split(cadr(car disco))"/"))(+ (length(string-split ruta "/")) 1))
                        (cons (car disco) (dir-ruta (cdr disco) ruta))
                        (dir-ruta (cdr disco) ruta)))))

;Función que ayuda a mostrar la ruta actual sin subcarpetas y sin archivos ocultos
(define dir-ruta-h (lambda (disco ruta)
                     (if(null? disco)
                        null
                        (if(and(eq? (length(string-split(cadr(car disco))"/"))(+ (length(string-split ruta "/")) 1))(not(verificar (flatten (car disco)) #\h)))
                           (cons (car disco) (dir-ruta-h (cdr disco) ruta))
                           (dir-ruta-h (cdr disco) ruta)))))

;Función que ayuda a mostrar toda la ruta actual, sin los archivos ocultos 
(define dir-ruta-t-h (lambda (disco ruta)
                       (if(null? disco)
                          null
                          (if(and(>= (length(string-split(cadr(car disco))"/"))(+ (length(string-split ruta "/")) 1))(not(verificar (flatten (car disco)) #\h)))
                             (cons (car disco) (dir-ruta-t-h (cdr disco) ruta))
                             (dir-ruta-t-h (cdr disco) ruta)))))

;Función que ayuda a mostrar toda la ruta actual
(define dir-ruta-t (lambda (disco ruta)
                     (if(null? disco)
                        null
                        (if(>= (length(string-split(cadr(car disco))"/"))(+ (length(string-split ruta "/")) 1))
                           (cons (car disco) (dir-ruta-t (cdr disco) ruta))
                           (dir-ruta-t (cdr disco) ruta)))))

;Función que ayuda a mostrar la ruta actual en oreden alfabetico
(define ordenar-alfa(lambda(carpetas)
                      (sort carpetas
                            (lambda (carpeta1 carpeta2)
                              (string<? (cadr carpeta1) (cadr carpeta2))))))

;Función que ayuda a mostrar la ruta actual ordenando por fechas en orden ascendente
(define nume-asc(lambda (carpetas)
                  (sort carpetas comparar-fecha-hora)))

;Función que ayuda a mostrar la ruta actual ordenando por fechas en orden ascendente
(define(comparar-fecha-hora elem1 elem2)
  (let*((fecha-y-hora1 (car(cdddr elem2)))(fecha-y-hora2 (car(cdddr elem2)))(longitud1 (string-length fecha-y-hora1))(longitud2 (string-length fecha-y-hora2)))
    (cond ((< longitud1 19) #f)((< longitud2 19) #t)
          (else(let* ((fecha1 (substring fecha-y-hora1 0 10))(hora1 (substring fecha-y-hora1 11 19))(fecha2 (substring fecha-y-hora2 0 10))(hora2 (substring fecha-y-hora2 11 19)))(or(and(string fecha1 fecha2)(string<? hora1 hora2))(string<? fecha1 fecha2)))))))

;Función que ayuda a eliminar la carpeta original antes de encriptarla
(define eli-encr-car(lambda (carpetas elemento system)
                      (if(null? carpetas)
                         null
                         (if(= (length (car carpetas)) 7)
                            (eli-encr-car (cdr carpetas) elemento system)
                            (cons(car carpetas)(eli-encr-car (cdr carpetas)elemento system))))))

;Función que ayuda a encriptar carpetas y todo su contenido
(define encriptar-car(lambda(carpetas encry decry contra elemento system)
                       (if(null? carpetas)
                          null
                          (if(= (length (car carpetas)) 7)
                             (if(equal? (encry "b") "c")
                                (cons(list(car(car carpetas))(cadr(car carpetas))(remplazar(caddr(car carpetas))2(plus-one(caddr(caddr(car carpetas)))))(cadddr(car carpetas))(list "plus-one" "minus-one" contra)(list-ref(list-ref system 0)1) null)(encriptar-car(cdr carpetas)encry decry contra elemento system))
                                (cons(list(car(car carpetas))(cadr(car carpetas))(remplazar(caddr(car carpetas))2(minus-one(caddr(caddr(car carpetas)))))(cadddr(car carpetas))(list "minus-one" "plus-one" contra)(list-ref(list-ref system 0)1) null)(encriptar-car(cdr carpetas)encry decry contra elemento system)))
                             (encriptar-car(cdr carpetas)encry decry contra elemento system)))))

;Función que ayuda a eliminar el archivo original antes de encriptarlo
(define eli-encr-ar(lambda (carpetas elemento system)
                     (if(null? carpetas)
                        null
                        (if(verificar (flatten(car carpetas)) elemento)
                           (eli-encr-ar (cdr carpetas) elemento system)
                           (cons (car carpetas)(eli-encr-ar (cdr carpetas) elemento system))))))

;Función que ayuda a encriptar archivos
(define encriptar-ar(lambda(carpetas encry decry contra elemento system)
                      (if(null? carpetas)
                         null
                         (if(verificar (flatten(car carpetas)) elemento)
                            (if(equal? (encry "b") "c")
                               (cons (list (car(car carpetas)) (cadr(car carpetas)) (remplazar (caddr(car carpetas)) 2(plus-one (caddr(caddr (car carpetas))))) (cadddr(car carpetas))(list "plus-one" "minus-one" contra) (list-ref(list-ref system 0)1) null)(encriptar-ar (cdr carpetas) encry decry contra elemento system))
                               (cons (list (car(car carpetas)) (cadr(car carpetas)) (remplazar (caddr(car carpetas))2(minus-one (caddr(caddr (car carpetas))))) (cadddr(car carpetas))(list "minus-one" "plus-one" contra) (list-ref(list-ref system 0)1) null)(encriptar-ar (cdr carpetas) encry decry contra elemento system)))
                            (encriptar-ar (cdr carpetas) encry decry contra elemento system)))))

;Función que ayuda a eliminar el archivo original antes de desencriptarlo
(define eli-decry-ar(lambda(carpetas elemento system)
                      (if(null? carpetas)
                         null
                         (if(verificar (flatten(car carpetas)) elemento)
                            (eli-decry-ar (cdr carpetas) elemento system)
                            (cons (car carpetas)(eli-decry-ar (cdr carpetas) elemento system))))))

;Función que ayuda a desencriptar archivos
(define decry-ar(lambda (carpetas contra elemento system)
                  (if(null? carpetas)
                     null
                     (if(verificar(flatten(car carpetas))elemento)
                        (if(equal? (car(fifth(car carpetas))) "plus-one")
                           (if(equal? (caddr(fifth(car carpetas)))contra)
                              (cons(list(car(car carpetas)) (cadr(car carpetas)) (remplazar (caddr(car carpetas))2(minus-one (caddr(caddr(car carpetas))))) (cadddr(car carpetas)) null (list-ref(list-ref system 0)1) null)(decry-ar (cdr carpetas) contra elemento system))
                              (cons (car carpetas)(decry-ar (cdr carpetas) contra elemento system)))
                           (if(equal? (caddr(fifth(car carpetas)))contra)
                              (cons(list(car(car carpetas)) (cadr(car carpetas)) (remplazar (caddr(car carpetas))2(plus-one (caddr(caddr(car carpetas))))) (cadddr(car carpetas)) null (list-ref(list-ref system 0)1) null)(decry-ar (cdr carpetas) contra elemento system))
                              (cons (car carpetas)(decry-ar(cdr carpetas)contra elemento system))))
                        (decry-ar (cdr carpetas) contra elemento system)))))

;Función que ayuda a eliminar la carpeta original antes de desencriptarla
(define eli-decry-car(lambda (carpetas elemento system)
                       (if(null? carpetas)
                          null
                          (if(= (length (car carpetas)) 7)
                             (eli-decry-car (cdr carpetas) elemento system)
                             (cons (car carpetas)(eli-decry-car (cdr carpetas) elemento system))))))

;Función que ayuda a desencriptar carpetas y todo su contenido
(define decry-car(lambda (carpetas contra elemento system)
                   (if(null? carpetas)
                      null
                      (if(= (length (car carpetas))7)
                         (if(equal? (car(fifth(car carpetas))) "plus-one")
                            (if(equal? (caddr(fifth(car carpetas)))contra)
                               (cons(list(car(car carpetas))(cadr(car carpetas))(remplazar (caddr(car carpetas))2(minus-one (caddr(caddr(car carpetas))))) (cadddr(car carpetas)) null (list-ref(list-ref system 0)1) null)(decry-car (cdr carpetas) contra elemento system))
                               (cons(car carpetas)(decry-car(cdr carpetas)contra elemento system)))
                            (if(equal?(caddr(fifth(car carpetas))) contra)
                               (cons(list(car(car carpetas))(cadr(car carpetas))(remplazar(caddr(car carpetas))2(plus-one (caddr(caddr(car carpetas))))) (cadddr(car carpetas)) null (list-ref(list-ref system 0)1) null)(decry-car (cdr carpetas) contra elemento system))
                               (cons(car carpetas)(decry-car(cdr carpetas)contra elemento system))))
                         (decry-car(cdr carpetas)contra elemento system)))))

(define plus-one(lambda(palabra)
                  (list->string (ascii+(string->list palabra)))))

;Función que ayuda a aplicar la suma al codigo ASCII
(define ascii+(lambda (lista)
                (if(null? lista)
                   null
                   (cons(integer->char(+ (char->integer(car lista)) 1))(ascii+ (cdr lista))))))

(define minus-one(lambda (palabra)
                   (list->string (ascii-(string->list palabra)))))

;Función que ayuda a aplicar la resta al codigo ASCII
(define ascii-(lambda (lista)
                (if(null? lista)
                   null
                   (cons(integer->char(- (char->integer(car lista)) 1))(ascii- (cdr lista))))))

;Función que ayuda a mostrar el string resultando cuando se busca en un archivo
(define grep-ayuda-ar(lambda (carpetas palabra system)
                       (if(null? carpetas)
                          null
                          (string-append "Las ocuurencias de la palabra "palabra " son: "(number->string(count (lambda (x) (string-ci=? x palabra))(string-split(caddr(caddr(car carpetas)))" ")))"\n""La o las posiciones de esta son: " (string-join (map number->string(reverse(foldl (lambda (s acc)(if (string-ci=? s palabra)(cons (length acc) acc)acc))'()(string-split(caddr(caddr(car carpetas)))" "))))", ")"\n"))))

;Función que ayuda a mostrar el string resultando cuando se busca en una ruta 
(define grep-ayuda-car(lambda (carpetas palabra system)
                        (if(null? carpetas)
                           null
                           (string-append "Las ocuurencias de la palabra "palabra " son: "(number->string(contador carpetas palabra system))"\n""La o las posiciones de esta son: " (contador-pos (cdr carpetas) palabra system)"\n"))))

;Función que ayuda encontrar la posicion del string que pide por comando en una ruta
(define contador-pos(lambda(carpetas palabra system)
                      (if(null? carpetas)
                         " "
                         (if(= (length(car carpetas)) 7)
                            (string-append(string-append (car(caddr(car carpetas)))": "(string-join (map number->string(reverse(foldl (lambda (s acc)(if (string-ci=? s palabra)(cons (length acc) acc) acc))'()(string-split(caddr(caddr(car carpetas)))" "))))", " )" ")(contador-pos (cdr carpetas) palabra system))
                            (contador-pos (cdr carpetas) palabra system)))))

;Función que ayuda encontrar la cuantas veces se repite del string que pide por comando en una ruta
(define contador(lambda (carpetas palabra system)
                  (if(null? carpetas)
                     0
                     (if(= (length(car carpetas)) 7)
                        (+ (count (lambda (x) (string-ci=? x palabra))(string-split(caddr(caddr(car carpetas)))" "))(contador (cdr carpetas) palabra system))
                        (contador (cdr carpetas) palabra system)))))

;Funcion que ayuda a mostrar de una mejor forma los string resultantes
(define (my-display obj)
  (write obj)
  (display "\n"))

;Función que ayuda a restaurar una carpeta de la papelera al get-carpetas
(define agregar-car(lambda (condicion disco)
                     (if(null? disco)
                        null
                        (if(and(= (length(car disco)) 6)(verificar-str(string-split(cadr(car disco))"/")condicion))
                           (cons (car disco) (agregar-car condicion (cdr disco)))
                           (agregar-car condicion(cdr disco))))))

;Función que ayuda a eliminar una carpeta de la papelera 
(define eliminar-car(lambda (disco)
                      (if(null? disco)
                         null
                         (if(= (length(car disco)) 7)
                            (cons (car disco) (eliminar-car (cdr disco)))
                            (eliminar-car (cdr disco))))))

;Función que ayuda a eliminar un archivo de la papelera
(define eliminar-ar(lambda (disco condicion)
                     (if(null? disco)
                        null
                        (if(verificar (flatten(car disco)) condicion)
                           (eliminar-ar (cdr disco) condicion)
                           (cons (car disco)(eliminar-ar (cdr disco) condicion))))))

;Función que ayuda a restaurar un archivo de la papelera al get-carpetas
(define agregar-ar(lambda (disco condicion)
                    (if(null? disco)
                       null
                       (if(verificar (flatten(car disco)) condicion)
                          (cons (car disco)(agregar-ar (cdr disco) condicion))
                          (agregar-ar (cdr disco) condicion)))))

;Función que ayuda a restaurar todo los elementos de la papelera, agregandolos al get-carpetas
(define agregar-todo(lambda (disco)
                      (if(null? disco)
                         null
                         (cons (car disco)(agregar-todo (cdr disco))))))