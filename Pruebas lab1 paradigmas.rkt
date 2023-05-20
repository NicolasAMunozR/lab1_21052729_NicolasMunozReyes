#lang racket

(require "labmain_21052729_MunozReyes.rkt")

(define file(lambda(nombre tipo contenido . caracteres)
              (list (string-downcase nombre)(string-downcase tipo)contenido caracteres)))

(define S0 (system "newSystem"))

(define S1 ((run S0 add-drive) #\c "SO" 1000))

(define S2 ((run S1 add-drive) #\c "SO1" 3000))

(define S3 ((run S2 add-drive) #\d "Util" 2000))

(define S4 ((run S3 register) "user1"))

(define S5 ((run S4 register) "user1"))

(define S6 ((run S5 register) "user2"))

(define S7 ((run S6 login) "user1"))

(define S8 ((run S7 login) "user2"))

(define S9 (run S8 logout))

(define S10 ((run S9 login) "User2"))

(define S11 ((run S10 switch-drive) #\K))

(define S12 ((run S11 switch-drive) #\c))

(define S13 ((run S12 md) "folder1"))

(define S14 ((run S13 md) "folder2"))

(define S15 ((run S14 md) "folder2"))

(define S16 ((run S15 md) "folder3"))

(define S17 ((run S16 cd) "folder2"))

(define S18 ((run S17 md) "folder21"))

(define S19 ((run S18 md) "folder21"))

(define S20 ((run S19 cd) "folder21"))

(define S21 ((run S20 cd) "folder22"))

(define S22 ((run S21 cd) ".."))

(define S23 ((run S22 cd) "folder21"))

(define S24 ((run S23 md) "folder211"))

(define S25 ((run S24 cd) "folder211"))

(define S26 ((run S25 cd) "/"))

(define S27 ((run S26 switch-drive) #\D))

(define S28 ((run S27 md) "folder5"))

(define S29 ((run S28 cd) "folder5"))

(define S30 ((run S29 cd) "C:/folder1/"))

(define S31 ((run S30 format) #\D "newD"))

(define S32 ((run S31 add-file) (file "foo1.txt" "txt" "hello world 1")))

(define S33 ((run S32 add-file) (file "foo2.txt" "txt" "hello world 2")))

(define S34 ((run S33 add-file) (file "foo3.docx" "docx" "hello world 3")))

(define S35 ((run S34 add-file) (file "goo4.docx" "docx" "hello world 4" #\h #\r)))

(define S36 ((run S35 del) "*.txt"))

(define S37 ((run S35 del) "f*.docx"))

(define S38 ((run S35 del) "goo4.docx"))

(define S39 ((run S35 cd) ".."))

(define S40 ((run S35 del) "folder1" ))

(define S41 ((run S39 rd) "folder1")) 

(define S42 ((run S41 cd) "folder1"))

(define S43 ((run S42 del) "*.*"))

(define S44 ((run S43 cd) ".."))

(define S45 ((run S44 rd) "Folder1"))

(define S46 ((run S35 copy) "foo1.txt" "C:/folder3/"))

(define S47 ((run S46 cd) ".."))

(define S48 ((run S47 copy) "folder1" "D:/"))

(define S49 ((run S48 move) "folder3"  "D:/"))

(define S50 ((run S49 cd) "folder1"))

(define S51 ((run S50 move) "foo3.docx" "D:/folder3/"))

(define S52 ((run S51 ren) "foo1.txt" "newFoo1.txt"))

(define S53 ((run S52 ren) "foo2.txt" "newFoo1.txt"))

(define S54 ((run S53 cd) ".."))

(define S55 ((run S54 ren) "folder1" "newFolder1"))

(newline)
(newline)
(display ((run S16 dir)))
(newline)
(newline)
(display ((run S55 dir)))
(newline)
(newline)
(display ((run S55 dir) "/a"))
(newline)
(newline)
(display ((run S55 dir) "/s /a"))
(newline)
(newline)

(define S56 ((run S55 encrypt) plus-one minus-one "1234" "newFolder1"))

(define S57 ((run S56 switch-drive) #\D))

(define S58 ((run S57 cd) "folder3"))

(define S59 ((run S58 encrypt) plus-one minus-one "4321" "foo3.docx"))

(define S60 ((run S59 decrypt) "1234" "foo3.docx"))

(define S61 ((run S60 decrypt) "4321" "foo3.docx"))

(define S62 ((run S61 switch-drive) #\C))

(define S63 ((run S62 decrypt) "1234" "newFolder1"))

(define S64 ((run S63 cd) "newFolder1"))

(newline)
(newline)
(display ((run S64 grep) "hello" "newFoo1.txt"))
(newline)
(newline)
(display ((run S64 grep) "hello" "*.*"))
(newline)
(newline)

(newline)
(newline)
(display (run S45 viewTrash))
(newline)
(newline)

(define S65 ((run S45 restore) "folder1"))

;Función Formar
(define S66 ((run S36 format) #\c "newc"))

;Función system
(define S67 (system "newSystem1"))
(define S68 (system "ElProgreso"))
(define S69 (system "E"))

;Función Add-drive
(define S70 ((run S3 add-drive) #\D "Util" 2000))
(define S71 ((run S67 add-drive) #\d "Si" 2000))
(define S72 ((run S71 add-drive) #\C "No" 3000))

;Función Register
(define S73 ((run S72 register) "user1"))
(define S74 ((run S73 register) "user1"))
(define S75 ((run S74 register) "USeR1"))

;Función Login
(define S76 ((run S75 login) "user1"))
(define S77 ((run S76 login) "user1"))

;Función Logout
(define S78 (run S77 logout))

;Función Login
(define S79 ((run S78 login) "UsaeR1"))

;Función Logout
(define S80 (run S66 logout))

;Función Login
(define S81 ((run S79 login) "USeR1"))

;Función Logout
(define S82 (run S81 logout))

;Función Login
(define S83 ((run S82 login) "USER1"))

;Función Switch-drive
(define S84 ((run S83 switch-drive) #\D))
(define S85 ((run S84 switch-drive) #\C))
(define S86 ((run S85 switch-drive) #\d))

;Función Md
(define S87 ((run S86 md) "folder1"))
(define S88 ((run S87 md) "FOLDER1"))
(define S89 ((run S88 md) "folder2"))

;Función Cd
(define S90 ((run S89 cd) "folder2"))
(define S91 ((run S90 cd) "/"))
(define S92 ((run S91 cd) "d:/folder1/"))
(define S93 ((run S92 cd) "c:/folder1/"))

;Función Add-file
(define S94 ((run S93 add-file) (file "fo.txt" "txt" "hello world 1")))
(define S95 ((run S94 add-file) (file "foo.docx" "docx" "hello world bye 1" #\h #\r)))

;Función Cd
(define S96 ((run S95 cd) "d:/FoldeR2/"))

;Función Add-file
(define S97 ((run S96 add-file) (file "foo1.txt" "txt" "hello world bye 1" #\h #\r)))

;Función Cd
(define S98 ((run S97 cd) ".."))

;Función Del
(define S99 ((run S98 del) "*.TXt"))
(define S100 ((run S98 del) "F*.docx"))
(define S101 ((run S98 del) "FoldeR1" ))

;Función Rd
(define S102 ((run S98 rd) "FoldER1")) 

;Función Del
(define S103 ((run S102 del) "*.*"))

;Función Rd
(define S104 ((run S103 rd) "FoldER1"))
(define S105 ((run S104 rd) "folder1"))

;Función Copy
(define S106 ((run S98 copy) "Folder1" "c:/"))
(define S107 ((run S106 copy) "folder1" "d:/Folder2/"))

;Función Cd
(define S108 ((run S107 cd) "d:/FoldeR2/"))

;Función Copy
(define S109 ((run S108 copy) "FoldeR1" "d:/"))

;Función Move
(define S110 ((run S109 move) "fo.txT"  "c:/"))

;Función Cd
(define S111 ((run S110 cd) ".."))

;Función Move
(define S112 ((run S111 move) "Folder2"  "c:/"))
(define S113 ((run S112 move) "folder3"  "D:/"))

;Función Cd
(define S114 ((run S113 cd) "c:/"))

;Función Ren
(define S115 ((run S114 ren) "fo.txt" "newFo.txt"))
(define S116 ((run S115 ren) "FOLDer2" "newFolder2"))
(define S117 ((run S116 ren) "Folder1" "Folder3"))

;Función Dir
(newline)
(newline)
(display ((run S117 dir) "/o n"))
(newline)
(newline)
(display ((run S117 dir) "/?"))
(newline)
(newline)
(display ((run S117 dir) "/o D"))
(newline)
(newline)

;Función Formar
(define S118 ((run S117 format) #\c "Si"))
(define S119 ((run S118 format) #\d "No"))

;Función Encrypt
(define S120 ((run S117 encrypt) plus-one minus-one "1234" "newFolder2"))

;Función Switch-drive
(define S121 ((run S120 switch-drive) #\D))

;Función Encrypt
(define S122 ((run S121 encrypt) minus-one plus-one "1334" "fo.Txt"))
(define S123 ((run S122 encrypt) plus-one minus-one "1234" "Foo.Docx"))

;Función Decrypt
(define S124 ((run S123 decrypt) "1334" "fo.Txt"))

;Función Switch-drive
(define S125 ((run S124 switch-drive) #\c))

;Función Decrypt
(define S126 ((run S125 decrypt) "4321" "newFolder2"))
(define S127 ((run S126 decrypt) "1234" "newFolder2"))

;Función Grep
(newline)
(newline)
(display ((run S127 grep) "bye" "*.*"))
(newline)
(newline)
(display ((run S127 grep) "hello" "newFo.txt"))
(newline)
(newline)
(display ((run S124 grep) "hello" "*.*"))
(newline)
(newline)

;Función ViewTrash
(newline)
(newline)
(display (run S99 viewTrash))
(newline)
(newline)
(display (run S100 viewTrash))
(newline)
(newline)
(display (run S101 viewTrash))
(newline)
(newline)

;Función Restore
(define S128 ((run S105 restore) "Folder1"))
(define S129 ((run S103 restore) "*.*"))
(define S130 ((run S100 restore) "folder1"))
