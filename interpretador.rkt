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

    ; Número negativo decimal:  -3.14
    (numero-token (("-" (arbno digit) "." (arbno digit))) number)
    ; Número negativo entero:   -5
    (numero-token (("-" (arbno digit))) number)
    ; Número positivo decimal:  3.14
    (numero-token (((arbno digit) "." (arbno digit))) number)
    ; Número positivo entero:   3
    (numero-token (((arbno digit))) number)

    ; Texto: empieza con letra, sigue con letras/dígitos/guión_bajo
    ; Las comillas NO van aquí, se manejan en la gramática
    ; Válidos: hola  FLP  mi_var2
    (texto-token ((letter (arbno (or letter digit "_")))) string)

    ; Identificador: SIEMPRE empieza con @
    ; Válidos: @x  @suma  @mi_var
    (identificador-token ("@" letter (arbno (or letter digit "_"))) symbol)
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