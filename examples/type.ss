#lang planet jaymccarthy/parenlog

(type Gamma numConst num)
(type Gamma boolConst bool)

(:- (type Gamma (if Test Then Else) Tau)
    (type Gamma Test bool)
    (type Gamma Then Tau)
    (type Gamma Else Tau))

(? (type mt numConst num)) ; => yes
(? (type mt (if boolConst numConst numConst) num)) ; => yes
(? (type mt (if boolConst numConst boolConst) num)) ; => no

(? (type mt boolConst T)) ; => T=bool
(? (type mt (if boolConst numConst numConst) T)) ; => T=num
(? (type mt (if boolConst numConst boolConst) T)) ; => no

(? (type mt T num)) ; => T=numConst

(type (bind V T Gamma) (var V) T)
(:- (type (bind V_ T_ Gamma) (var V) T)
    (type Gamma (var V) T))

(? (type (bind w bool (bind v num mt))
         (var v)
         T))
; => T=num

(:- (type Gamma (fun Var Body) (T1 -> T2))
    (type (bind Var T1 Gamma) Body T2))

(? (type mt (fun x (if (var x) numConst boolConst)) T)) ; => no

(? (type mt (fun x (if (var x) numConst numConst)) T)) ; => T=(bool -> num)

(:- (type Gamma (app Fun Arg) T2)
    (type Gamma Fun (T1 -> T2))
    (type Gamma Arg T1))

(? (type mt
         (app (fun x (if (var x) numConst numConst))
              boolConst)
         T))
; => T=num

(? (type mt (fun x (var x)) T))
; => T=(T1213424 -> T1213424)

(? (type mt
         (app (fun id
                   (if (app (var id) boolConst)
                       (app (var id) boolConst)
                       (app (var id) boolConst)))
              (fun x (var x)))
         T))
; => T=bool

(? (type mt
         (app (fun id
                   (if (app (var id) boolConst)
                       (app (var id) numConst)
                       (app (var id) numConst)))
              (fun x (var x)))
         T))
; => no

(? (type mt (fun x (app (var x) (var x))) num))
; => no

; Infinite loop:
#;(? (type mt (fun x (app (var x) (var x))) T))