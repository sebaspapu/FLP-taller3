#lang eopl
#|
Integrantes:
- Jairo Hernan Gonzalez Barreto - 202324314
- Sebastian Bolaños Morales -202310168

Repositorio GitHub:
 https://github.com/sebaspapu/FLP-taller3
|#

; ============================================================
; ESPECIFICACIÓN LÉXICA
; Define los tokens (unidades mínimas) que reconoce el lenguaje.
; El scanner lee el texto de izquierda a derecha y agrupa caracteres.
; ============================================================

(define especificacion-lexica
  '(
    ; Espacios y saltos de línea: se ignoran completamente
    (espacio-blanco (whitespace) skip)

    ; Números: SLLGEN maneja enteros, decimales y negativos
    ; automáticamente con el tipo "number"
    (numero-token (digit (arbno digit)) number)

    ; Texto: empieza con letra, sigue con letras/dígitos/guión_bajo
    ; Las comillas NO van aquí, se manejan en la gramática
    ; Válidos: hola  FLP  mi_var2
    (texto-token (letter (arbno (or letter digit #\_))) string)

    ; Identificador: SIEMPRE empieza con @
    ; Válidos: @x  @suma  @mi_var
    (identificador-token ("@" letter (arbno (or letter digit #\_))) symbol)
  ))

; ============================================================
; ESPECIFICACIÓN GRAMATICAL
; Aqui defino cómo se combinan los tokens para formar expresiones.
; Cada regla produce un nodo del AST (árbol sintáctico abstracto).
; ============================================================

(define especificacion-gramatical
  '(
    ; PROGRAMA RAÍZ: un programa es una expresión
    (<programa>
     (<expresion>)
     un-programa)

    ; NÚMERO LITERAL: cualquier número del lexer
    (<expresion>
     (numero-token)
     numero-lit)

    ; TEXTO LITERAL: entre comillas dobles
    ; El lexer ya extrajo el contenido sin comillas (texto-token)
    (<expresion>
     ("\"" texto-token "\"")
     texto-lit)

    ; VARIABLE: identificador que empieza con @
    (<expresion>
     (identificador-token)
     var-exp)

    ; OPERACIÓN BINARIA INFIJA: (exp1 OP exp2)
    ; Ejemplo: (3 + 4)  (@x ~ 2)  (@d concat @e)
    (<expresion>
     ("(" <expresion> <primitiva-binaria> <expresion> ")")
     primapp-bin-exp)

    ; OPERACIÓN UNARIA PREFIJA: OP(exp)
    ; Ejemplo: longitud(@d)  add1(3)  neg(0)
    (<expresion>
     (<primitiva-unaria> "(" <expresion> ")")
     primapp-un-exp)

    ; CONDICIONAL (punto 4)
    ; Si <cond> { <verdadero> } sino { <falso> }
    (<expresion>
     ("Si" <expresion> "{" <expresion> "}" "sino" "{" <expresion> "}")
     condicional-exp)

    ; VARIABLES LOCALES (punto 5)
    ; declarar (@x=2; @y=3;) { cuerpo }
    ; arbno = cero o más repeticiones del patrón
    (<expresion>
     ("declarar" "(" (arbno identificador-token "=" <expresion> ";") ")" "{" <expresion> "}")
     variableLocal-exp)

    ; PROCEDIMIENTO / LAMBDA (punto 6)
    ; procedimiento (@x, @y) { cuerpo }
    ; separated-list = lista separada por comas
    (<expresion>
     ("procedimiento" "(" (separated-list identificador-token ",") ")" "{" <expresion> "}")
     procedimiento-exp)

    ; APLICACIÓN / LLAMADO (punto 7)
    ; evaluar @f (arg1, arg2) finEval
    (<expresion>
     ("evaluar" <expresion> "(" (separated-list <expresion> ",") ")" "finEval")
     app-exp)

    ; RECURSIÓN (punto 8)
    ; recursivo @nombre (@params) = { cuerpo } en { uso }
    (<expresion>
     ("recursivo" identificador-token
      "(" (separated-list identificador-token ",") ")"
      "=" "{" <expresion> "}"
      "en" "{" <expresion> "}")
     letrec-exp)

    ; ---- PRIMITIVAS BINARIAS ----
    (<primitiva-binaria> ("+")      primitiva-suma)
    (<primitiva-binaria> ("~")      primitiva-resta)       ; ~ para restar
    (<primitiva-binaria> ("/")      primitiva-div)
    (<primitiva-binaria> ("*")      primitiva-multi)
    (<primitiva-binaria> ("concat") primitiva-concat)      ; concatenar strings
    (<primitiva-binaria> (">")      primitiva-mayor)
    (<primitiva-binaria> ("<")      primitiva-menor)
    (<primitiva-binaria> (">=")     primitiva-mayor-igual)
    (<primitiva-binaria> ("<=")     primitiva-menor-igual)
    (<primitiva-binaria> ("!=")     primitiva-diferente)
    (<primitiva-binaria> ("==")     primitiva-comparador-igual)

    ; ---- PRIMITIVAS UNARIAS ----
    (<primitiva-unaria> ("longitud") primitiva-longitud)
    (<primitiva-unaria> ("add1")     primitiva-add1)
    (<primitiva-unaria> ("sub1")     primitiva-sub1)
    (<primitiva-unaria> ("neg")      primitiva-negacion-booleana)
  ))

; ============================================================
; CONSTRUCCIÓN DEL PARSER CON SLLGEN
; sllgen genera automáticamente el scanner+parser a partir
; de las especificaciones léxica y gramatical.
; También genera los define-datatype de los nodos del AST.
; ============================================================

; Genera los datatypes: expresion, programa, primitiva-binaria, primitiva-unaria
(sllgen:make-define-datatypes especificacion-lexica especificacion-gramatical)

; Genera la función scanner&parser que convierte string -> AST
(define scanner&parser
  (sllgen:make-string-parser especificacion-lexica especificacion-gramatical))


; ============================================================
; DATATYPE PARA PROCEDIMIENTOS (ProcVal)
; Una cerradura guarda 3 cosas:
;   1. lista-ID:  los nombres de los parámetros  ej: '(@x @y)
;   2. exp:       el cuerpo (AST, sin evaluar)
;   3. amb:       el ambiente donde fue DECLARADO
; El ambiente capturado permite el alcance léxico (lexical scoping).
; ============================================================

(define-datatype procVal procVal?
  (cerradura
   (lista-ID (list-of symbol?))
   (exp (lambda (x) #t))       ; acepta cualquier valor como cuerpo
   (amb (lambda (x) #t))       ; acepta cualquier valor como ambiente
   ))

; ============================================================
; DATATYPE PARA EL AMBIENTE
; El ambiente es una pila de marcos (frames).
; Cada marco asocia una lista de nombres con una lista de valores.
; ============================================================

; Predicado auxiliar: cualquier valor de Scheme es aceptable
(define scheme-value? (lambda (v) #t))

(define-datatype ambiente ambiente?

  ; Ambiente base: sin ninguna variable
  (ambiente-vacio)

  ; Extiende un ambiente con nuevas variables y sus valores
  (ambiente-extendido
   (ids  (list-of symbol?))
   (vals (list-of scheme-value?))
   (amb-anterior (lambda (x) #t)))   ; acepta cualquier ambiente

  ; Ambiente para recursión: el nombre de la función se ve a sí mismo
  (ambiente-recursivo
   (nombre symbol?)
   (params (list-of symbol?))
   (cuerpo (lambda (x) #t))          ; acepta cualquier cuerpo
   (amb-anterior (lambda (x) #t)))   ; acepta cualquier ambiente
  )


; ============================================================
; ============================================================


; ============================================================
; AMBIENTE INICIAL
; @a=1, @b=2, @c=3, @d="hola", @e="FLP"
; ============================================================

(define ambiente-inicial
  (ambiente-extendido
   '(@a  @b  @c  @d      @e)
   '(1    2   3  "hola"  "FLP")
   (ambiente-vacio)))

; ============================================================
; BUSCAR-VARIABLE
; Recorre el ambiente buscando el identificador.
; Retorna el valor si lo encuentra, error si no.
; ============================================================

(define buscar-variable
  (lambda (id amb)
    (cases ambiente amb

      (ambiente-vacio ()
        (eopl:error 'buscar-variable "Error, la variable ~s no existe" id))

      (ambiente-extendido (ids vals amb-anterior)
        (buscar-en-listas id ids vals amb-anterior))

      (ambiente-recursivo (nombre params cuerpo amb-anterior)
        (if (equal? id nombre)
            (cerradura params cuerpo amb)
            (buscar-variable id amb-anterior)))
      )))

(define buscar-en-listas
  (lambda (id ids vals amb-anterior)
    (cond
      ((null? ids)
       (buscar-variable id amb-anterior))
      ((equal? id (car ids))
       (car vals))
      (else
       (buscar-en-listas id (cdr ids) (cdr vals) amb-anterior))
      )))


; ============================================================
; ============================================================

; ============================================================
; VALOR-VERDAD?
; 0 = falso, cualquier otro número = verdadero
; ============================================================

(define valor-verdad?
  (lambda (val)
    (not (= val 0))))

; ============================================================
; EVALUAR PRIMITIVA BINARIA
; En #lang eieo las primitivas son símbolos, se comparan con equal?
; ============================================================

(define evaluar-primitiva-binaria
  (lambda (prim val1 val2)
    (cond
      ((equal? prim 'primitiva-suma)            (+ val1 val2))
      ((equal? prim 'primitiva-resta)           (- val1 val2))
      ((equal? prim 'primitiva-div)
       (if (= val2 0)
           (eopl:error 'evaluar-primitiva-binaria "División por cero")
           (/ val1 val2)))
      ((equal? prim 'primitiva-multi)           (* val1 val2))
      ((equal? prim 'primitiva-concat)          (string-append val1 val2))
      ((equal? prim 'primitiva-mayor)           (if (> val1 val2)          1 0))
      ((equal? prim 'primitiva-menor)           (if (< val1 val2)          1 0))
      ((equal? prim 'primitiva-mayor-igual)     (if (>= val1 val2)         1 0))
      ((equal? prim 'primitiva-menor-igual)     (if (<= val1 val2)         1 0))
      ((equal? prim 'primitiva-diferente)       (if (not (equal? val1 val2)) 1 0))
      ((equal? prim 'primitiva-comparador-igual)(if (equal? val1 val2)     1 0))
      (else (eopl:error 'evaluar-primitiva-binaria "Primitiva desconocida: ~s" prim))
      )))

; ============================================================
; EVALUAR PRIMITIVA UNARIA
; ============================================================

(define evaluar-primitiva-unaria
  (lambda (prim val)
    (cond
      ((equal? prim 'primitiva-longitud)
       (if (string? val)
           (string-length val)
           (eopl:error 'primitiva-longitud "Esperaba string, recibió: ~s" val)))
      ((equal? prim 'primitiva-add1)              (+ val 1))
      ((equal? prim 'primitiva-sub1)              (- val 1))
      ((equal? prim 'primitiva-negacion-booleana) (if (valor-verdad? val) 0 1))
      (else (eopl:error 'evaluar-primitiva-unaria "Primitiva desconocida: ~s" prim))
      )))