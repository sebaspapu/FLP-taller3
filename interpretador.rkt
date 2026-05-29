#lang eopl
#|
Integrantes:
- Jairo Hernan Gonzalez Barreto - 202324314
- Sebastian Bolaños Morales -202310168

Repositorio GitHub:
 https://github.com/sebaspapu/FLP-taller3
|#

; °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
; ESPECIFICACIÓN LÉXICA
; Define los tokens (unidades mínimas) que reconoce el lenguaje.
; El scanner lee el texto de izquierda a derecha y agrupa caracteres.

(define especificacion-lexica
  '(
    ; Espacios y saltos de línea: se ignoran completamente
    (espacio-blanco (whitespace) skip)
 
    ; Enteros positivos
    (numero-token
     (digit (arbno digit))
     number)

    ; Enteros negativos
    (numero-token
     ("-" digit (arbno digit))
     number)

    ;Decimales positivos
    (numero-token
     (digit (arbno digit) "." digit (arbno digit))
     number)

    ; Decimales negativos
    (numero-token
     ("-" digit (arbno digit) "." digit (arbno digit))
     number)
 
    ; Identificador: siempre empieza con @
    ; Válidos: @x  @suma  @mi_var
    (identificador-token ("@" letter (arbno (or letter digit "_"))) symbol)
 
    ; Texto entre comillas dobles
    (texto-token
    (letter (arbno (or letter digit "_" ":" "!" "?" "." "," "-")))
    string)
    
  ))

; °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
; ESPECIFICACIÓN GRAMATICAL
; Aqui defino cómo se combinan los tokens para formar expressiones.
; Cada regla produce un nodo del AST (árbol sintáctico abstracto).


(define especificacion-gramatical
  '(
    ; PROGRAMA RAÍZ: un programa es una expresión
    (<program>
     (<expression>)
     un-program)
 
    ; NÚMERO LITERAL
    (<expression>
     (numero-token)
     numero-lit)
 
    ; Texto literal entre comillas
    (<expression>
     ("\"" texto-token "\"")
     texto-lit)
 
    ; VARIABLE: identificador que empieza con @
    (<expression>
     (identificador-token)
     var-exp)
 
    ; OPERACIÓN BINARIA INFIJA: (exp1 OP exp2)
    ; Ejemplo: (3 + 4)  (@x ~ 2)  (@d concat @e)
    (<expression>
     ("(" <expression> <primitiva_binaria> <expression> ")")
     primapp-bin-exp)
 
    ; OPERACIÓN UNARIA PREFIJA: OP(exp)
    ; Ejemplo: longitud(@d)  add1(3)  neg(0)
    (<expression>
     (<primitiva_unaria> "(" <expression> ")")
     primapp-un-exp)
 
    ; CONDICIONAL: Si <cond> { <verdadero> } sino { <falso> }
    (<expression>
     ("Si" <expression> "{" <expression> "}" "sino" "{" <expression> "}")
     condicional-exp)
 
    ; VARIABLES LOCALES: declarar (@x=2; @y=3;) { cuerpo }
    (<expression>
     ("declarar" "(" (arbno identificador-token "=" <expression> ";") ")" "{" <expression> "}")
     variableLocal-exp)
 
    ; PROCEDIMIENTO / LAMBDA: procedimiento (@x, @y) { cuerpo }
    (<expression>
     ("procedimiento" "(" (separated-list identificador-token ",") ")" "{" <expression> "}")
     procedimiento-exp)
 
    ; APLICACIÓN: evaluar @f (arg1, arg2) finEval
    (<expression>
     ("evaluar" <expression> "(" (separated-list <expression> ",") ")" "finEval")
     app-exp)
 
    ; RECURSIÓN: recursivo @nombre (@params) = { cuerpo } en { uso }
    (<expression>
     ("recursivo" identificador-token
      "(" (separated-list identificador-token ",") ")"
      "=" "{" <expression> "}"
      "en" "{" <expression> "}")
     letrec-exp)
 
    ; ---- PRIMITIVAS BINARIAS ----
    (<primitiva_binaria> ("+")      primitiva-suma)
    (<primitiva_binaria> ("~")      primitiva-resta)
    (<primitiva_binaria> ("/")      primitiva-div)
    (<primitiva_binaria> ("*")      primitiva-multi)
    (<primitiva_binaria> ("concat") primitiva-concat)
    (<primitiva_binaria> (">")      primitiva-mayor)
    (<primitiva_binaria> ("<")      primitiva-menor)
    (<primitiva_binaria> (">=")     primitiva-mayor-igual)
    (<primitiva_binaria> ("<=")     primitiva-menor-igual)
    (<primitiva_binaria> ("!=")     primitiva-diferente)
    (<primitiva_binaria> ("==")     primitiva-comparador-igual)
 
    ; ---- PRIMITIVAS UNARIAS ----
    (<primitiva_unaria> ("longitud") primitiva-longitud)
    (<primitiva_unaria> ("add1")     primitiva-add1)
    (<primitiva_unaria> ("sub1")     primitiva-sub1)
    (<primitiva_unaria> ("neg")      primitiva-negacion-booleana)
  ))


; °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
; DATATYPES MANUALES
; Se definen manualmente porque sllgen:make-define-datatypes
; no funciona correctamente en Racket 9.0 con #lang racket.
; El parser sllgen sí funciona correctamente.

; Genera los datatypes: expression, programa, primitiva-binaria, primitiva-unaria
; Datatype para primitivas binarias: cada variante representa un operador
(define-datatype primitiva_binaria primitiva_binaria?
  (primitiva-suma)
  (primitiva-resta)
  (primitiva-div)
  (primitiva-multi)
  (primitiva-concat)
  (primitiva-mayor)
  (primitiva-menor)
  (primitiva-mayor-igual)
  (primitiva-menor-igual)
  (primitiva-diferente)
  (primitiva-comparador-igual))
 
; Datatype para primitivas unarias: cada variante representa un operador
(define-datatype primitiva_unaria primitiva_unaria?
  (primitiva-longitud)
  (primitiva-add1)
  (primitiva-sub1)
  (primitiva-negacion-booleana)
  (primitiva-piso))   ; convierte division exacta a entero
 
; Datatype principal: cada variante es un tipo de nodo del AST
(define-datatype expression expression?
  (numero-lit        (n number?))
  (texto-lit         (t string?))
  (var-exp           (id symbol?))
  (primapp-bin-exp   (exp1 expression?) (prim-bin primitiva_binaria?) (exp2 expression?))
  (primapp-un-exp    (prim-un primitiva_unaria?) (exp1 expression?))
  (condicional-exp   (test-exp expression?) (true-exp expression?) (false-exp expression?))
  (variableLocal-exp (ids (list-of symbol?)) (exps (list-of expression?)) (cuerpo expression?))
  (procedimiento-exp (ids (list-of symbol?)) (cuerpo expression?))
  (app-exp           (fun-exp expression?) (arg-exps (list-of expression?)))
  (letrec-exp        (nombre symbol?) (params (list-of symbol?)) (cuerpo-fun expression?) (cuerpo-en expression?))) 

; Datatype para el programa raíz
(define-datatype program program?
  (un-program (exp expression?)))

; °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
; PARSER
; Convierte un string en un AST usando las especificaciones
; léxica y gramatical definidas arriba.
 
(define scanner&parser
  (sllgen:make-string-parser especificacion-lexica especificacion-gramatical))


; °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
; DATATYPE PARA PROCEDIMIENTOS (ProcVal)
; Una cerradura guarda 3 cosas:
;   1. lista-ID:  los nombres de los parámetros  ej: '(@x @y)
;   2. exp:       el cuerpo (AST, sin evaluar)
;   3. amb:       el ambiente donde fue DECLARADO
; El ambiente capturado permite el alcance léxico (lexical scoping).

(define-datatype procVal procVal?
  (cerradura
   (lista-ID (list-of symbol?))
   (exp (lambda (x) #t))       ; acepta cualquier valor como cuerpo
   (amb (lambda (x) #t))       ; acepta cualquier valor como ambiente
   ))


; °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
; DATATYPE PARA EL AMBIENTE
; El ambiente es una pila de marcos (frames).
; Cada marco asocia una lista de nombres con una lista de valores.


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



; °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
; AMBIENTE INICIAL
; @a=1, @b=2, @c=3, @d="hola", @e="FLP"

(define ambiente-inicial
  (ambiente-extendido
   '(@a  @b  @c  @d      @e)
   '(1    2   3  "hola"  "FLP")
   (ambiente-vacio)))


; °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
; BUSCAR-VARIABLE
; Recorre el ambiente buscando el identificador dado.
; Retorna el valor asociado si lo encuentra.
; Lanza error si la variable no existe en ningún marco.

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

; Función auxiliar: recorre listas paralelas de ids y vals buscando id
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




; °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
; VALOR-VERDAD?
; 0 = falso, cualquier otro número = verdadero

(define valor-verdad?
  (lambda (val)
    (and (number? val) (not (= val 0)))))


; °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
; EVALUAR PRIMITIVA BINARIA
; En #lang eieo las primitivas son símbolos, se comparan con equal?


; REEMPLAZA evaluar-primitiva-binaria con esto:
(define evaluar-primitiva-binaria
  (lambda (prim val1 val2)
    (cases primitiva_binaria prim
      (primitiva-suma ()             (+ val1 val2))
      (primitiva-resta ()            (- val1 val2))
      (primitiva-div ()              (if (= val2 0)
                                         (eopl:error 'evaluar-primitiva-binaria "Division por cero")
                                         (quotient val1 val2)))  ; division entera
      (primitiva-multi ()            (* val1 val2))
      (primitiva-concat ()           (string-append val1 val2))
      (primitiva-mayor ()            (if (> val1 val2)            1 0))
      (primitiva-menor ()            (if (< val1 val2)            1 0))
      (primitiva-mayor-igual ()      (if (>= val1 val2)           1 0))
      (primitiva-menor-igual ()      (if (<= val1 val2)           1 0))
      (primitiva-diferente ()        (if (not (equal? val1 val2)) 1 0))
      (primitiva-comparador-igual () (if (equal? val1 val2)       1 0)))))

; REEMPLAZA evaluar-primitiva-unaria con esto:
(define evaluar-primitiva-unaria
  (lambda (prim val)
    (cases primitiva_unaria prim
      (primitiva-longitud ()
        (if (string? val)
            (string-length val)
            (eopl:error 'primitiva-longitud "Esperaba string, recibio: ~s" val)))
      (primitiva-add1 ()              (+ val 1))
      (primitiva-sub1 ()              (- val 1))
      (primitiva-negacion-booleana () (if (valor-verdad? val) 0 1))
      (primitiva-piso ()              (inexact->exact (floor val))))))



; °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
; APLICAR PROCEDIMIENTO
; Extiende el ambiente de DECLARACIÓN (no el de llamada).


(define aplicar-procedimiento
  (lambda (proc args)
    (cases procVal proc
      (cerradura (lista-ID cuerpo amb-declaracion)
        (if (not (= (length lista-ID) (length args)))
            (eopl:error 'aplicar-procedimiento
              "Aridad incorrecta: esperaba ~s args, recibio ~s"
              (length lista-ID) (length args))
            (evaluar-expression
             cuerpo
             (ambiente-extendido lista-ID args amb-declaracion)))))))


; °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
; EVALUADOR PRINCIPAL
; Toma un nodo del AST y un ambiente, retorna un valor.
; "cases" hace pattern matching sobre el datatype expression.


(define evaluar-expression
  (lambda (exp amb)
    (cases expression exp

      ; Número literal: retorna el número directamente
      (numero-lit (n) n)

      ; Texto literal: retorna el string directamente
      (texto-lit (t) t)

      ; Variable: busca en el ambiente
      (var-exp (id)
        (buscar-variable id amb))

      ; Binaria: evalúa ambos lados, aplica operador
      (primapp-bin-exp (exp1 prim-bin exp2)
        (let ((val1 (evaluar-expression exp1 amb))
              (val2 (evaluar-expression exp2 amb)))
          (evaluar-primitiva-binaria prim-bin val1 val2)))

      ; Unaria: evalúa el argumento, aplica operador
      (primapp-un-exp (prim-un exp1)
        (let ((val (evaluar-expression exp1 amb)))
          (evaluar-primitiva-unaria prim-un val)))

      ; Condicional: evalúa condición, escoge rama
      (condicional-exp (test-exp true-exp false-exp)
        (if (valor-verdad? (evaluar-expression test-exp amb))
            (evaluar-expression true-exp  amb)
            (evaluar-expression false-exp amb)))

      ; Variables locales:
      ; Evalúa las declaraciones secuencialmente para permitir
      ; closures correctos y alcance léxico.
      (variableLocal-exp (ids exps cuerpo)
      (let loop ((ids ids)
             (exps exps)
             (env amb))
         (if (null? ids)
           (evaluar-expression cuerpo env)
           (let ((val (evaluar-expression (car exps) env)))
             (loop (cdr ids)
                (cdr exps)
                (ambiente-extendido(list (car ids))(list val) env))))))

      ; Procedimiento: NO evalúa. Crea cerradura con ambiente actual.
      (procedimiento-exp (ids cuerpo)
        (cerradura ids cuerpo amb))

      ; Aplicación: evalúa función y argumentos, llama aplicar-procedimiento
      (app-exp (fun-exp arg-exps)
        (let ((fun  (evaluar-expression fun-exp amb))
              (args (map (lambda (e) (evaluar-expression e amb)) arg-exps)))
          (if (procVal? fun)
              (aplicar-procedimiento fun args)
              (eopl:error 'app-exp "Error: intento de aplicar un no-procedimiento"))))

      ; Recursión: crea ambiente-recursivo y evalúa el cuerpo-en
      (letrec-exp (nombre params cuerpo-fun cuerpo-en)
        (evaluar-expression
         cuerpo-en
         (ambiente-recursivo nombre params cuerpo-fun amb)))
      )))


; °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
; FUNCIÓN PRINCIPAL: interpretar
; Recibe un string, lo parsea y lo evalúa en el ambiente inicial.
; Es el punto de entrada del interpretador.


(define interpretar
  (lambda (string)
    (cases program (scanner&parser string)
      (un-program (exp)
        (evaluar-expression exp ambiente-inicial)))))


; °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
;Pruebas genericas


; °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
;PROGRAMAS DEL PUNTO 9
; °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
; A) Procedimiento @sumarDigitos
; Implementa la suma de dígitos de un número entero positivo usando recursión.
; Prueba: evaluar @sumarDigitos(147) finEval -> 12
#|

(interpretar
"
recursivo @residuo10(@n)= {
  Si (@n < 10) {
    @n
  } sino {
    (@n ~ ((@n / 10) * 10))
  }
}
en {
  recursivo @cociente10(@n)= {
    Si (@n < 10) {
      0
    } sino {
      ((@n ~ (@n ~ ((@n / 10) * 10))) / 10)
    }
  }
  en {
    recursivo @sumarDigitos(@n)= {
      Si (@n < 10) {
        @n
      } sino {
        (evaluar @residuo10(@n) finEval +
         evaluar @sumarDigitos(evaluar @cociente10(@n) finEval) finEval)
      }
    }
    en {
      evaluar @sumarDigitos(147) finEval
    }
  }
}
")

|#

; °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
; B) Procedimiento @factorial
; Calcula el factorial de un número n usando recursión.
; Caso base: factorial(0) = 1
; Caso recursivo: n * factorial(n-1)
; Pruebas:
; evaluar @factorial(5) finEval  -> 120
; evaluar @factorial(10) finEval -> 3628800

#|

(interpretar
"
recursivo @factorial(@n)= {
  Si (@n == 0) {
    1
  } sino {
    (@n * evaluar @factorial((@n ~ 1)) finEval)
  }
}
en {
  evaluar @factorial(5) finEval
}
")



(interpretar
"
recursivo @factorial(@n)= {
  Si (@n == 0) {
    1
  } sino {
    (@n * evaluar @factorial((@n ~ 1)) finEval)
  }
}
en {
  evaluar @factorial(10) finEval
}
")

|#

; °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
; C) Procedimiento @potencia
; evaluar @potencia(4,2) finEval -> 16

#|

(interpretar
"
recursivo @potencia(@base,@exponente)= {
  Si (@exponente == 0) {
    1
  } sino {
    (@base * evaluar @potencia(@base,(@exponente ~ 1)) finEval)
  }
}
en {
  evaluar @potencia(4,2) finEval
}
")

|#

; °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
; D) Suma de rango
; evaluar @sumaRango(2,5) finEval ->14

#|

(interpretar "
   recursivo @sumaRango(@a,@b)= {
      Si (@a == @b) {
         @a
      }sino {
         (@a + evaluar @sumaRango((@a + 1), @b) finEval)
      }
   }
   en {
      evaluar @sumaRango(2,5) finEval
   }
")

|#

; °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°

; E) Decorador
; @saludar recibe una función y retorna otra funcion que
; modifica su salida agregando el prefijo "Hola:"
; evaluar @decorate() finEval -> "Hola:Jairo_y_Sebastian"



; °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
; F) Decorador con mensaje final
; evaluar @decorate("_ProfesoresFLP") finEval
; debe retornar: "Hola:Jairo_y_Sebastian_EstudiantesFLP"


