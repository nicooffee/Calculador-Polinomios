#lang scheme
#|
/*--------------------------------Trabajo de laboratorio 2: Operaciones de polinomios--------------------------------**/
/***********************************************************************************************************************
#    Autores   : NICOLAS IGNACIO HONORATO DROGUETT.
#    Programa  : Operaciones entre polinomios.
#    Proposito : Implementar las operaciones básicas entre polinomios de una sola variable.
#    Version   : DrRacket, version 7.0
#    Fecha     : Santiago de Chile, 31 de agosto de 2018.
***********************************************************************************************************************/
|#
(define A 'x^2-3/2x+3)
(define B '3x-5/4x^3-x^2+5)
(define C 'x+4)
(define D 'x^3-12x^2-42)
(define E 'x-3)
(define Z '0)
#|
# Aclaraciones:
#
# Las funciones para modificar los valores de cada polinomio utilizan una lista de pares de numeros,
# los cuales corresponden al coeficiente y al exponente de cada termino.
#
# Ejemplo: 3/2x^3 => (3/2 3), 3/2x^3+5x-7 => ((3/2 3) (5 1) (-7 0))
#
# Los simbolos generalmente tienen el nombre de psym. (letra mayuscula en el caso de las func. principales).
# Los simbolos transformados a pares usualmente son llamados -letra-list, ppairlist. EJ: plist, qlist.
# Los simbolos transformados a string usualmente son llamados pstrlist o pstr, para el caso de los
# elementos de la lista.
# Los elementos de la lista de simbolos transformados a pares tienen los nombres ppair, punit, -letra- minus.
#
|#

;*****************************FUNCIONES DE TRANSFORMACION DEL POLINOMIO*****************************;
#|
# Bloque transformación del polinomio.
#
# En este bloque se definen las funciones que manejan a los pares de numeros. Son las encargadas de
# realizar las operaciones básicas, pero solo de par a par.
#
|#
(define (exp tuple)
  (cadr tuple)
  )
(define (esc tuple)
  (car tuple)
  )
(define (cmpExp t1 t2); (e1<e2)? #t #f
  (< (exp t1) (exp t2))
  )
(define (multPunit P Q); P*Q
  (list (* (esc P) (esc Q)) (+ (exp P) (exp Q)))
  )
(define (divPunit P Q); P/Q
  (list (/ (esc P) (esc Q)) (- (exp P) (exp Q)))
  )
;***************************FIN FUNCIONES DE TRANSFORMACION DEL POLINOMIO***************************;


;*****************************FUNCIONES DE TRANSFORMACION DEL POLINOMIO*****************************;
;--------------------------------------FUNCIONES SIMBOLO->LISTA-------------------------------------;
#|
# Bloque funciones simbolo->lista:
#
# Las funciones que están dentro del bloque simbolo->lista son las encargas de transformar el
# símbolo que representa al polinomio a una lista de pares de coeficiente-exponente.
#
# Las funciones siguen el siguiente orden para ejecutar la transformación:
#
# ;trns: psym->pstr->plist(punit->ppair)->ppairlist
# Simbolo->String->String[]->(String[i]->Par coef-exp, i=[0,length(String[])] ∈ N+{0})->Lista pares coef-exp.
#
# La funcion principal de este bloque es la funcion psym->ppairlist.
#
|#

;psym->pstr: polynomial symbol -> polynomial string
(define (psym->pstr P)
  (cond
    ((eq? P '0) (make-string 1 #\0))
    ((number? P) (number->string P))
    (else (symbol->string P))
    )
  )


(define (reemplazarOperadores sP)
  (string-replace (string-replace sP "+" " ") "-" " -")
  )

;psym->plist: polynomial symbol -> polynomial unit list
(define (psym->plist P)
  (string-split (reemplazarOperadores (psym->pstr P)))
  )


;punit->ppair: polynomial unit -> polynomial pair
;ppair: (list scalar exponent)
(define (punit->ppair uP)
  (let
      ((ulist (string-split (string-replace (string-replace (string-replace uP "-x" "-1x") "^" " ") "x" " x")))
       )
    (cond
      ((= (length ulist) 3) (list (string->number (car ulist)) (string->number (caddr ulist))))
      ((and (string=? (car ulist) "x") (= (length ulist) 1)) (list 1 1))
      ((string=? (car ulist) "x") (list 1 (string->number (cadr ulist))))
      ((= (length ulist) 2) (list (string->number (car ulist)) 1))
      (else (list (string->number (car ulist)) 0))
      )
    )
  )

;plist->ppairlist: polynomial unit list -> polynomial pair list
(define (plist->ppairlist plist [ppairlist (list)])
  (if (pair? plist)
      (plist->ppairlist (cdr plist) (append ppairlist (list (punit->ppair (car plist)))))
      ppairlist
      )
  )
;psym->ppairlist: polynomial symbol -> polynomial pair list
(define (psym->ppairlist P)
  (plist->ppairlist (psym->plist P))
  )
;------------------------------------FIN FUNCIONES SIMBOLO->LISTA-----------------------------------;



;--------------------------------------FUNCIONES LISTA->SIMBOLO-------------------------------------;
#|
# Bloque funciones lista->simbolo:
#
# Las funciones de este bloque se encargan de invertir el proceso ejecutado por el bloque anterior,
# es decir, transforman una lista de pares a un simbolo de polinomio.
#
# La función principal de este bloque es ppairlist->psym.
#
|#

;ppair->pstr: polynomial pair -> polynomial string
(define (ppair->pstr ppair)
  (let
      ((s (esc ppair))
       (e (exp ppair))
       )
    (cond
      ((= s 0) "0")
      ((and (= s 1) (= e 1)) "x")
      ((and (= s -1) (= e 1)) "-x")
      ((= e 0) (number->string s))
      ((= s 1) (string-append "x^" (number->string e)))
      ((= s -1) (string-append "-x^" (number->string e)))
      ((= e 1) (string-append (number->string s) "x"))
      (else (string-append (number->string s) (string-append "x^" (number->string e))))
      )
    )
  )

;ppairlist->psym: polynomial pair list -> polynomial symbol
(define (ppairlist->psym pplist)
  (let*
      ((pstrlist (remove* (list "0") (map ppair->pstr (sort pplist cmpExp))))
       )
    (if (or (null? pplist) (null? pstrlist))
        '|0|
        (string->symbol
         (foldl
          (lambda (x y) (if (char=? (string-ref y 0) #\-)
                            (string-append x y)
                            (string-append x (string-append "+" y))
                            )
            )
          (car pstrlist) (cdr pstrlist)
          )
         )
        )
    )
  )

;------------------------------------FIN FUNCIONES LISTA->SIMBOLO-----------------------------------;     
;***************************FIN FUNCIONES DE TRANSFORMACION DEL POLINOMIO***************************;


;***************************************FUNCIONES PRINCIPALES***************************************;
#|
# Bloque funciones principales:
#
# Las funciones de este bloque son las que se encargan de realizar las operaciones al polinomio,
# recibiendo el/los simbolo/s inicial/es y retornando el resultado en forma de símbolo.
#
# Además, se encuentran todas las sub-funciones necesarias para la ejecución
#
|#
;--------------------------------------------POLY-DEGREE--------------------------------------------;
#|
# Sub-bloque, poly-degree:
#
# Se encarga de retornar el máximo exponente del polinomio P. Crea una lista de exponentes por medio
# de la funcion map y luego busca el valor máximo de esa lista.
#
|#
(define (poly-degree P)
  (let*
      ((pairlist (psym->ppairlist P))
       (lexp (map exp pairlist))
       )
    (apply max lexp)
    )
  )
;----------------------------------------------POLY-ADD---------------------------------------------;
#|
# Sub-bloque, poly-add:
#
# Se encarga de sumar dos polinomios P y Q. Para ello, llama a la función itSumTupl la cual se encarga
# de sumar la lista ltuple a sum. En itSumTupl se llama a la función sumTupl, la cual suma el par
# tuple a la lista sum.
#
# sumTupl posee tres casos para sumar,
#
# 1) Si la lista sum es nula, retorna una lista que contenga tuple.
# 2) Si encuentra un par con el mismo exponente dentro de sum, suma sus coeficientes.
# 3) Si no existe un polinomio con el mismo exponente, agrega a tuple al final de sum.
#
# OBS: La función poly-add permite un paso de parámetros variable, es decir:
#
# (poly-add A B)
# (poly-add A B C D E)
#
# son igual de válidos.
#
|#

(define (sumTupl sum tuple) ;sumar tuple a algun elemento de la lista sum
  (cond
    ((null? sum) (list tuple))
    ((= (exp (car sum)) (exp tuple))
     (let
         ((sumUnit (+ (esc tuple) (esc (car sum))))
          )
       (if (= sumUnit 0)
           (cdr sum)
           (cons (list sumUnit (exp tuple)) (cdr sum))
           )
       )
     )
    (else (cons (car sum) (sumTupl (cdr sum) tuple)))
    )
  )

(define (itSumTupl sum ltuple) ;suma la lista ltuple a la lista sum
    (if (pair? ltuple)
        (itSumTupl (sumTupl sum (car ltuple)) (cdr ltuple))
        sum
        )
    )


(define (poly-add P Q . resto)
  (let*
      ((plist (psym->ppairlist P))
       (qlist (psym->ppairlist Q))
       )
    (ppairlist->psym (foldl itSumTupl (itSumTupl plist qlist) (map psym->ppairlist resto)))
    )
  )
;-------------------------------------------POLY-MULTIPLY-------------------------------------------;
#|
# Sub-bloque, poly-multiply:
#
# Las funciones de este bloque se encargan de multiplicar un polinomio P por uno Q.
#
# Para ello, se utiliza la función multPolys que multiplica todos los elementos de
# una lista de pares plist, por una lista de pares qlist. Cada iteración de multPolys
# hace una llamada a multPoly, para multiplicar una lista qlist por un par p.
#
# Además, se hacen llamadas a las funciones del bloque anterior, para sumar los
# resultados de multPoly al acumulador total.
#
|#
(define (multPoly p qlist [sum (list)]) ;multiplicar qlist por p
      (cond
        ((null? qlist) sum)
        (else
         (multPoly p (cdr qlist) (sumTupl sum (multPunit (car qlist) p)))
         )
        )
      )
(define (multPolys plist qlist [sum (list)]) ;multipliciar todos los elementos de plist por todos los elementos de qlist
    (cond
      ((null? plist) sum)
      (else
       (multPolys (cdr plist) qlist (itSumTupl sum (multPoly (car plist) qlist)))
       )
      )
    )
(define (poly-multiply P Q)
  (let*
      ((plist (psym->ppairlist P))
       (qlist (psym->ppairlist Q))
       )
    (ppairlist->psym (multPolys plist qlist))
    )
  )
;-------------------------------------------POLY-DIVISION-------------------------------------------;
#|
# Sub-bloque, poly-division:
#
# Las funciones de este bloque se encargan de dividir la lista plist por la lista qlist.
#
# Para realizar la división, se utiliza el algoritmo de la Regla de Ruffini.
#
# Las funciones de este bloque llaman a las funciones para sumar y multiplicar que fueron explicadas
# anteriormente.
#
# La funcion divPolys retorna una lista la cual tiene una forma de (resto cuociente).
#
#
|#
(define (cambiarSigno plist)
  (map (lambda (p) (list (- (esc p)) (exp p))) plist)
  )


(define (divPolys plist qlist c [acum (list)])
  (cond
    ((null? plist) (sort (append  (list (list 0 0)) acum) cmpExp)) ;Caso plist vacío: dividir al polinomio nulo o la división fue exacta (resto=0)
    (else
     (let
         ((q (car (reverse (sort qlist cmpExp))))
          (p (car (reverse (sort plist cmpExp))))
          )
       (cond
         ((> (exp q) (exp p))
          (cond
            ((= c 1) acum)
            (else plist)
            )
          )
         (else
          (let
              ((pdivq (divPunit p q))
               )
            (divPolys (itSumTupl plist (cambiarSigno (multPoly pdivq qlist))) qlist c (append acum (list pdivq)))
            )
          )
         )
       )
     )
    )
  )
;-------------------------------------------POLY-QUOTIENT-------------------------------------------;
#|
# Sub-bloque, poly-quotient:
#
# La función de este bloque se encarga de retornar el cuociente de la divisón entre P y Q, llamando
# a la función divPolys.
#
|#

(define (poly-quotient P Q)
  (let*
      ((plist (psym->ppairlist P))
       (qlist (psym->ppairlist Q))
       )
    (cond
      ((< (poly-degree P) (poly-degree Q)) '|0|)
      (else (ppairlist->psym (divPolys plist qlist 1)))
      )
    )
  )
;-------------------------------------------POLY-REMAINDER------------------------------------------;
#|
# Sub-bloque, poly-remainder:
#
# La función de este bloque se encarga de retornar el resto de la divisón entre P y Q, llamando
# a la función divPolys.
#
|#
(define (poly-remainder P Q)
  (let*
      ((plist (psym->ppairlist P))
       (qlist (psym->ppairlist Q))
       )
    (cond
     ((< (poly-degree P) (poly-degree Q)) P)
     (else (ppairlist->psym (divPolys plist qlist 0)))
     )
    )
  )
;---------------------------------------------POLY-EVAL---------------------------------------------;
#|
# Sub-bloque, poly-eval:
#
# La función de este bloque se encarga de "evaluar" x en P, generando así una sumatoria de los
# resultados de multiplicar y elevar x por cada par de la lista plist, que corresponde al símbolo P.
#
|#
(define (poly-eval P x)
    (apply + (map (lambda (t) (* (car t) (expt x (cadr t)))) (psym->ppairlist P)))
  )
;*************************************FIN FUNCIONES PRINCIPALES*************************************;
(poly-degree 'x^2-3/2x+3) 
(poly-degree '3x-5/4x^3-x^2+5) 
(poly-degree 'x+4) 
(poly-add 'x+4 'x^2-x+1)
(poly-add 'x^2+1/3x+4 '2/3x)
(poly-multiply 'x-1 'x^2+x+1)
(poly-multiply 'x^3+6x^2+5 '2/3) 
(poly-quotient 'x^3 'x-1) 
(poly-remainder 'x^3 'x-1)
(poly-eval 'x^2-x+1 1)
(poly-eval 'x^2-x+1 1/2)