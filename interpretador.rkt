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
   (exp expresion?)
   (amb ambiente?)
   ))

; ============================================================
; DATATYPE PARA EL AMBIENTE
; El ambiente es una pila de marcos (frames).
; Cada marco asocia una lista de nombres con una lista de valores.
; ============================================================

(define-datatype ambiente ambiente?

  ; Ambiente base: sin ninguna variable
  (ambiente-vacio)

  ; Extiende un ambiente con nuevas variables y sus valores
  ; ids:  lista de símbolos, ej: '(@x @y)
  ; vals: lista de valores,  ej: '(2 3)
  ; amb-anterior: el ambiente que queda debajo
  (ambiente-extendido
   (ids  (list-of symbol?))
   (vals (list-of scheme-value?))
   (amb-anterior ambiente?))

  ; Ambiente para recursión: el nombre de la función se ve a sí mismo
  (ambiente-recursivo
   (nombre symbol?)
   (params (list-of symbol?))
   (cuerpo expresion?)
   (amb-anterior ambiente?))
  )

; Predicado auxiliar: cualquier valor de Scheme es aceptable
(define scheme-value? (lambda (v) #t))