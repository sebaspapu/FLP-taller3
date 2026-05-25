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

