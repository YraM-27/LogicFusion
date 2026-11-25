#lang eopl

;;ESTUDIANTES:
;;Sergio Ernesto Patiño Rodriguez 2440051
;;Paula Mariana Murillo Huertas 2436488
;;Juan Diego Ledezma 2540088
;;María José Gonzalez Rosero 2439500

;;Definición BNF para las expresiones del lenguaje:
;;
;;<programa>    ::= "GLOBALS" "{" {<declaracion>}* "}" "PROGRAM" "{" "proc" "main" "(" ")" <secuencia> "}"
;;                  <un-programa (decls cuerpo)>
;;<declaracion> ::= <tipo> <identificador> "=" <expresion> ";"
;;                  <declaracion-var (t id exp)>
;;              ::= "const" <tipo> <identificador> "=" <expresion> ";"
;;                  <declaracion-const (t id exp)>
;;              ::= "proc" <identificador> "=" "lambda" "(" {<tipo> <identificador>}*(",") ")" <secuencia>
;;                  <declaracion-proc (id tipos ids cuerpo)>
;;<tipo>        ::= int | float | string | bool | void | sat
;;              ::= list "<" <tipo> ">" | vector "<" <tipo> ">" | dict "<" <tipo> "," <tipo> ">"
;;<expresion>   ::= <aritmetica> {<op-rel> <aritmetica>}* <resto-asig>
;;                  <rel-exp (a ops bs asig)>
;;<resto-asig>  ::= "->" <expresion>
;;                  <con-asignacion (exp)>
;;              ::= (vacío)
;;                  <sin-asignacion ()>
;;<resto-id>    ::= {"(" <expresion>**(",") ")"}*
;;                  <resto-llamadas (argss)>
;;<aritmetica>  ::= <termino> {<op-ad> <termino>}*
;;                  <ad-exp (t ops ts)>
;;<termino>     ::= <factor> {<op-mul> <factor>}*
;;                  <mul-exp (f ops fs)>
;;<factor>      ::= <numero> | <cadena> | True | False | "(" <expresion> ")"
;;              ::= <primitiva-unaria> "(" <expresion> ")"
;;              ::= "print" "(" <expresion>**(",") ")"
;;              ::= <identificador> <resto-id>
;;<op-rel>      ::= > | < | >= | <= | != | ==
;;<op-ad>       ::= + | - | concat
;;<op-mul>      ::= * | /
;;<secuencia>   ::= "{" <sentencia> {<sentencia>}* "}"
;;                  <una-secuencia (s ss)>
;;<sentencia>   ::= <expresion> ";"
;;                  <sentencia-exp (exp)>
;;              ::= <estructura>
;;                  <sentencia-estructura (est)>
;;              ::= "return" <retorno>
;;                  <sentencia-return (r)>
;;<retorno>     ::= <expresion> ";"
;;                  <retorno-exp (exp)>
;;              ::= <estructura>
;;                  <retorno-estructura (est)>
;;<estructura>  ::= "BLOCK" <secuencia>
;;                  <block-est (sec)>
;;              ::= "LOCALS" "{" <declaracion> {<declaracion>}* "}" <secuencia>
;;                  <locals-est (d ds sec)>
;;              ::= "if" <expresion> <secuencia> "else" <secuencia>
;;                  <if-est (test sec-v sec-f)>
;;              ::= "switch" <expresion> "{" {"case" <expresion> ":" <secuencia>}* "default" ":" <secuencia> "}"
;;                  <switch-est (exp vals secs default)>
;;              ::= "while" <expresion> <secuencia>
;;                  <while-est (test sec)>
;;              ::= "for" "(" <expresion> ";" <expresion> ";" <expresion> ")" <secuencia>
;;                  <for-est (init test update sec)>              
;;<primitiva-unaria>  ::= longitud (primitiva-longitud)
;;                    ::= add1 (primitiva-add1)
;;                    ::= sub1 (primitiva-sub1)
;;                    ::= neg (primitiva-negacion-booleana)


;;Especificacion Lexica

(define especificacion-lexica
  '((espacio-blanco
     (whitespace) skip)
    (comentario
     ("%" (arbno (not #\newline))) skip)
    (identificador
     (letter (arbno (or letter digit "_" "?"))) symbol)
    (numero
     (digit (arbno digit)) number)
    (numero
     ("-" digit (arbno digit)) number)
    (numero
     (digit (arbno digit) "." digit (arbno digit)) number)
    (numero
     ("-" digit (arbno digit) "." digit (arbno digit)) number)
    (cadena
     ("\"" (arbno (not #\")) "\"") string)))

;;Especificación Sintáctica (gramática)

(define gramatica
  '((programa ("GLOBALS" "{" (arbno declaracion) "}" "PROGRAM" "{" "proc" "main" "(" ")" secuencia "}") un-programa)
    (declaracion ("proc" identificador "=" "lambda" "(" (separated-list tipo identificador ",") ")" secuencia) declaracion-proc)
    (declaracion (tipo identificador "=" expresion ";") declaracion-var)
    (declaracion ("const" tipo identificador "=" expresion ";") declaracion-const)
    
    (tipo ("int") tipo-int)
    (tipo ("float") tipo-float)
    (tipo ("string") tipo-string)
    (tipo ("bool") tipo-bool)
    (tipo ("void") tipo-void)
    (tipo ("sat") tipo-sat)
    (tipo ("list" "<" tipo ">") tipo-list)
    (tipo ("vector" "<" tipo ">") tipo-vector)
    (tipo ("dict" "<" tipo "," tipo ">") tipo-dict)
    
    (expresion (aritmetica (arbno op-rel aritmetica) resto-asig) rel-exp)
    (aritmetica (termino (arbno op-ad termino)) ad-exp)
    (termino (factor (arbno op-mul factor)) mul-exp)

    (factor (numero) numero-lit)
    (factor (cadena) cadena-lit)
    (factor ("True") true-exp)
    (factor ("False") false-exp)
    (factor ("(" expresion ")") parentesis-exp)
    (factor (primitiva-unaria "(" expresion ")") primapp-un-exp)
    (factor ("print" "(" (separated-list expresion ",") ")") print-exp)
    (factor (identificador resto-id) id-exp)
    

    (op-rel (">") primitiva-mayor)
    (op-rel ("<") primitiva-menor)
    (op-rel (">=") primitiva-mayor-igual)
    (op-rel ("<=") primitiva-menor-igual)
    (op-rel ("!=") primitiva-diferente)
    (op-rel ("==") primitiva-comparador-igual)
    (op-ad ("+") primitiva-suma)
    (op-ad ("-") primitiva-resta)
    (op-ad ("concat") primitiva-concat)
    (op-mul ("*") primitiva-multi)
    (op-mul ("/") primitiva-div)
    
    (resto-id ((arbno "(" (separated-list expresion ",") ")")) resto-llamadas)
    (resto-asig ("->" expresion) con-asignacion)
    (resto-asig () sin-asignacion)
    
    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
    (primitiva-unaria ("neg") primitiva-negacion-booleana)

    (secuencia ("{" sentencia (arbno sentencia) "}") una-secuencia)
    (sentencia (expresion ";") sentencia-exp)
    (sentencia (estructura) sentencia-estructura)
    (sentencia ("return" retorno) sentencia-return)
    (retorno (expresion ";") retorno-exp)
    (retorno (estructura) retorno-estructura)

    (estructura ("BLOCK" secuencia) block-est)
    (estructura ("LOCALS" "{" declaracion (arbno declaracion) "}" secuencia) locals-est)
    (estructura ("if" expresion secuencia "else" secuencia) if-est)
    (estructura ("switch" expresion "{" (arbno "case" expresion ":" secuencia) "default" ":" secuencia "}") switch-est)
    (estructura ("while" expresion secuencia) while-est)
    (estructura ("for" "(" expresion ";" expresion ";" expresion ")" secuencia) for-est)
    
    ;; --- listas: producciones aquí ---
    
    
    ;; --- vectores: producciones aquí ---

    (factor ("["(separated-list expresion ",") "]" ) vec-exp)
    (factor ("make-vector" "(" numero ")" ) make-vector-exp)
    (factor ("ref-vector" "(" expresion "," expresion ")") ref-vec-exp)
    (factor("set-vector" "(" expresion "," expresion "," expresion ")") set-vec-exp)

    (primitiva-unaria ("vector?") primitiva-vec?)
    


    
    ;; --- diccionarios: producciones aquí ---
    
    ;; --- SAT: producciones aquí ---
    
    ))

;;Tipos de datos para la sintaxis abstracta de la gramática

(sllgen:make-define-datatypes especificacion-lexica gramatica)

;;show-the-datatypes
;;No recibe argumentos y muestra la lista de datatypes que sllgen
;;genera a partir de la léxica y la gramática.
;;Sirve para verificar los nombres de los constructores del árbol de sintaxis abstracta

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes especificacion-lexica gramatica)))

;;Parser y scanner

;;scan&parse
;;Recibe un string con un programa y devuelve su árbol de sintaxis abstracta.
;;Hace el análisis léxico y sintáctico en un solo paso con la léxica y la gramática.
;;Se usa para probar el parser, por ejemplo: (scan&parse "GLOBALS { int a = 10; } PROGRAM { proc main() { return a; } }")

(define scan&parse
  (sllgen:make-string-parser especificacion-lexica gramatica))

;;just-scan
;;Recibe un string con un programa y devuelve la lista de tokens que reconoce
;;la especificación léxica, sin construir el árbol.
;;Sirve para revisar si la léxica reconoce bien números, cadenas e identificadores

(define just-scan
  (sllgen:make-string-scanner especificacion-lexica gramatica))

;;Interpretador
;;Inicia el ciclo de lectura-evaluación-impresión (REPL) con el prompt "-->".
;;Lee cada programa escrito, lo convierte en árbol con el parser y lo evalúa
;;con eval-programa. Es la función que se ejecuta para usar el lenguaje.

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-programa  pgm)) 
    (sllgen:make-stream-parser 
      especificacion-lexica
      gramatica)))

;;eval-programa
;;Evalúa GLOBALS para construir el ambiente global y luego ejecuta main.

(define eval-programa
  (lambda (pgm)
    (cases programa pgm
      (un-programa (decls cuerpo-main)
                   (let ((amb-global (evaluar-declaraciones decls (ambiente-vacio))))
                     (evaluar-secuencia cuerpo-main amb-global))))))

;;evaluar-declaracion
;;Evalúa una declaración y devuelve el ambiente extendido con ella.
;;Para proc: primero se crea la variable, luego la cerradura captura el ambiente que
;;ya contiene esa variable, y por último se guarda la cerradura en la referencia.
;;Así el procedimiento se encuentra a sí mismo y la recursión funciona por defecto.

(define evaluar-declaracion
  (lambda (decl env)
    (cases declaracion decl
      (declaracion-var (t id exp)
        (extender-ambiente-var (list id)
                               (list (validar-tipo t id (evaluar-expresion exp env)))
                               (list t) (list #f) env))
      (declaracion-const (t id exp)
        (extender-ambiente-var (list id)
                               (list (validar-tipo t id (evaluar-expresion exp env)))
                               (list t) (list #t) env))
      (declaracion-proc (id tipos ids cuerpo)
        (let* ((nuevo-env (extender-ambiente-var (list id) (list 0)
                                                 (list (tipo-void)) (list #f) env))
               (ref (car (buscar-info id nuevo-env))))
          (setref! ref (cerradura tipos ids cuerpo nuevo-env))
          nuevo-env)))))

;;evaluar-declaraciones
;;Evalúa una lista de declaraciones en orden, cada una con el ambiente de la anterior.

(define evaluar-declaraciones
  (lambda (decls env)
    (if (null? decls)
        env
        (evaluar-declaraciones (cdr decls)
                               (evaluar-declaracion (car decls) env)))))

;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
;;Tiene dos variantes: vacio (ambiente sin variables) y extendido, que guarda los
;;símbolos, sus referencias (vectores de un elemento con el valor mutable), sus tipos,
;;si cada variable es constante y el ambiente padre. La recursión no necesita una
;;variante aparte: al declarar un proc, su cerradura captura el ambiente que ya
;;contiene la variable del propio procedimiento, así se puede llamar a sí mismo.

(define-datatype ambiente ambiente?
  (vacio)
  (extendido (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (tipos (list-of tipo?))
                       (consts (list-of boolean?))
                       (amb ambiente?)))
  

(define scheme-value? (lambda (v) #t))

;ambiente-vacio:      -> ambiente
;función que crea un ambiente vacío

(define ambiente-vacio  
  (lambda ()
    (vacio)))       ;llamado al constructor de ambiente vacío 

;extender-ambiente-var: <list-of symbols> <list-of valores> <list-of tipos> <list-of boolean> ambiente -> ambiente
;;Crea un ambiente extendido; cada valor se guarda en una referencia nueva (crear-ref).

;extender-ambiente: <list-of symbols> <list-of valores> ambiente -> ambiente
;;Versión corta: las variables quedan de tipo void y no constantes.

(define extender-ambiente-var
  (lambda (syms vals tipos consts amb)
    (extendido syms (map crear-ref vals) tipos consts amb)))

(define extender-ambiente
  (lambda (syms vals amb)
    (extender-ambiente-var syms vals
                           (map (lambda (s) (tipo-void)) syms)
                           (map (lambda (s) #f) syms)
                           amb)))

;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de un ambiente

(define buscar-posicion
  (lambda (sym los)
    (indice-lista (lambda (sym1) (eqv? sym1 sym)) los)))

(define indice-lista
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((indice-lista-r (indice-lista pred (cdr ls))))
              (if (number? indice-lista-r)
                (+ indice-lista-r 1)
                #f))))))

;;crear-ref / deref / setref!
;;Una referencia es un vector de un solo elemento que guarda el valor mutable.

(define crear-ref (lambda (v) (vector v)))
(define deref (lambda (r) (vector-ref r 0)))
(define setref! (lambda (r v) (vector-set! r 0 v)))


;******************************************************************************************

;;buscar-info: devuelve (list referencia tipo const?) de la variable

(define buscar-info
  (lambda (sym env)
    (cases ambiente env
      (vacio () (eopl:error 'buscar-variable "La variable ~s no existe" sym))
      (extendido (syms vals tipos consts amb)
                 (let ((pos (buscar-posicion sym syms)))
                   (if (number? pos)
                       (list (list-ref vals pos) (list-ref tipos pos) (list-ref consts pos))
                       (buscar-info sym amb)))))))


;;buscar-variable
;;Recibe un símbolo y un ambiente y devuelve el valor actual de la variable.
;;Si no existe, buscar-info lanza un error.

(define buscar-variable
  (lambda (sym env)
    (deref (car (buscar-info sym env)))))

;;valor-coincide-tipo?
;;Verifica si el valor v corresponde al tipo t (void no se valida). float acepta enteros.

(define valor-coincide-tipo?
  (lambda (t v)
    (cases tipo t
      (tipo-int () (and (number? v) (integer? v) (exact? v)))
      (tipo-float () (number? v))
      (tipo-string () (string? v))
      (tipo-bool () (boolean? v))
      (tipo-void () #t)
      (tipo-sat () (instancia-sat? v))
      (tipo-list (t1) (and (list? v) (todos-coinciden? t1 v)))
      (tipo-vector (t1) (and (vector-lf? v) (todos-coinciden? t1 (contenido-vector v))))
      (tipo-dict (tk tv) (and (dict-lf? v) (pares-coinciden? tk tv (pares-dict v)))))))

;;todos-coinciden?: cada elemento coincide con t (las posiciones sin inicializar se aceptan).

(define todos-coinciden?
  (lambda (t lst)
    (cond ((null? lst) #t)
          ((or (eq? (car lst) 'sin-valor) (valor-coincide-tipo? t (car lst)))
           (todos-coinciden? t (cdr lst)))
          (else #f))))

;;pares-coinciden?: cada clave y cada valor coinciden con los tipos del diccionario.

(define pares-coinciden?
  (lambda (tk tv pares)
    (cond ((null? pares) #t)
          ((and (valor-coincide-tipo? tk (car (car pares)))
                (valor-coincide-tipo? tv (cdr (car pares))))
           (pares-coinciden? tk tv (cdr pares)))
          (else #f))))

(define void-tipo?
  (lambda (t) (cases tipo t (tipo-void () #t) (else #f))))

;;valor-por-defecto: valor inicial de cada posición de make-vector según el tipo de elementos.

(define valor-por-defecto
  (lambda (t)
    (cases tipo t
      (tipo-int () 0)
      (tipo-float () 0.0)
      (tipo-string () "")
      (tipo-bool () #f)
      (tipo-void () 0)
      (tipo-list (t1) '())
      (else 'sin-valor))))

;;preparar-valor
;;Si el valor es un vector o diccionario que aún no tiene tipo, le asigna el declarado
;;(y en el vector rellena las posiciones de make-vector con el valor por defecto).

(define preparar-valor
  (lambda (t v)
    (cases tipo t
      (tipo-vector (t1)
                   (if (vector-lf? v)
                       (cases vector-lf v
                         (un-vector (tref eref)
                                    (begin
                                      (if (void-tipo? (deref tref))
                                          (begin
                                            (setref! tref t)
                                            (setref! eref (map (lambda (x) (if (eq? x 'sin-valor) (valor-por-defecto t1) x))
                                                               (deref eref))))
                                          #f)
                                      v)))
                       v))
      (tipo-dict (tk tv)
                 (if (dict-lf? v)
                     (cases dict-lf v
                       (un-dict (tref pref)
                                (begin
                                  (if (void-tipo? (deref tref)) (setref! tref t) #f)
                                  v)))
                     v))
      (else v))))

;;validar-tipo: devuelve v (ya preparado) si coincide con el tipo, o lanza error

(define validar-tipo
  (lambda (t id v)
    (let ((v2 (preparar-valor t v)))
      (if (valor-coincide-tipo? t v2)
          v2
          (eopl:error 'tipo "El valor ~s no coincide con el tipo declarado de ~s" v id)))))

;;asignar-variable
;;Cambia el valor de una variable existente validando el tipo y bloqueando constantes.
;;La usa la asignación x -> exp (resto-asignacion).

(define asignar-variable
  (lambda (sym nuevo env)
    (let* ((info (buscar-info sym env))
           (ref (car info))
           (t (cadr info))
           (const? (caddr info)))
      (if const?
          (eopl:error 'asignacion "No se puede modificar la constante ~s" sym)
          (setref! ref (validar-tipo t sym nuevo))))))

;*******************************************************************************************

;;procVal
;;Tipo de dato que representa un procedimiento del lenguaje (cerradura).
;;Tiene cuatro campos: la lista de tipos de los parámetros, la lista de
;;identificadores (parámetros), el cuerpo (una secuencia de sentencias) y el ambiente donde
;;fue declarado. Se crea al evaluar una declaración proc y se usa en
;;aplicar-procedimiento para evaluar el cuerpo con los argumentos.

(define-datatype procVal procVal?
  (cerradura (tipos (list-of tipo?))
             (lista-ID (list-of symbol?))
             (cuerpo secuencia?)
             (amb ambiente?)))


;;*******************************************************************************************
;;Valores de las estructuras de datos y SAT

;;vector-lf: vector mutable. Guarda su tipo (en una referencia; void mientras no se declare)
;;y su contenido (en una referencia a una lista, para poder cambiar el tamaño).

(define-datatype vector-lf vector-lf?
  (un-vector (tipo-ref vector?) (elems-ref vector?)))

;;dict-lf: diccionario mutable. Guarda su tipo y una lista de pares (clave . valor).
(define-datatype dict-lf dict-lf?

  (un-dict (tipo-ref vector?) (pares-ref vector?)))

;;instancia-sat: número de variables y lista de cláusulas (cada una, una lista de enteros no nulos).

(define-datatype instancia-sat instancia-sat?
  (una-instancia (nvars integer?) (clausulas (list-of (list-of integer?)))))

(define contenido-vector (lambda (v) (cases vector-lf v (un-vector (t e) (deref e)))))
(define cambiar-contenido-vector! (lambda (v nuevo) (cases vector-lf v (un-vector (t e) (setref! e nuevo)))))
(define tipo-del-vector (lambda (v) (cases vector-lf v (un-vector (t e) (deref t)))))
(define pares-dict (lambda (d) (cases dict-lf d (un-dict (t p) (deref p)))))
(define cambiar-pares-dict! (lambda (d nuevo) (cases dict-lf d (un-dict (t p) (setref! p nuevo)))))
(define tipo-del-dict (lambda (d) (cases dict-lf d (un-dict (t p) (deref t)))))

;;no-implementada
;;Error que lanzan los esqueletos hasta que alguien los implemente.

(define no-implementada
  (lambda (nombre)
    (eopl:error nombre "Esta primitiva todavia no esta implementada")))

;;exigir-n: verifica la cantidad de argumentos de una primitiva.

(define exigir-n
  (lambda (nombre args n)
    (if (= (length args) n)
        #t
        (eopl:error nombre "Se esperaban ~s argumentos y se recibieron ~s" n (length args)))))

;;argumento: verifica la cantidad de argumentos y devuelve el k-ésimo.

(define argumento
  (lambda (nombre args n k)
    (begin (exigir-n nombre args n)
           (list-ref args k))))

(define exigir-lista
  (lambda (nombre v)
    (if (list? v) v (eopl:error nombre "Se esperaba una lista y se recibio ~s" v))))
(define exigir-vector
  (lambda (nombre v)
    (if (vector-lf? v) v (eopl:error nombre "Se esperaba un vector y se recibio ~s" v))))
(define exigir-dict
  (lambda (nombre v)
    (if (dict-lf? v) v (eopl:error nombre "Se esperaba un diccionario y se recibio ~s" v))))
(define exigir-entero
  (lambda (nombre v)
    (if (and (number? v) (integer? v) (exact? v))
        v
        (eopl:error nombre "Se esperaba un entero y se recibio ~s" v))))

;;exigir-posicion: verifica que 0 <= pos < limite y devuelve pos.

(define exigir-posicion
  (lambda (nombre pos limite)
    (if (and (exigir-entero nombre pos) (>= pos 0) (< pos limite))
        pos
        (eopl:error nombre "Posicion ~s fuera de rango" pos))))

                                               
;;evaluar-expresion
;;Evalúa una expresión: una comparación, o una asignación x -> exp cuando hay "->".
;;En la asignación, el lado izquierdo debe ser solo un identificador.

(define evaluar-expresion
  (lambda (exp env)
    (cases expresion exp
      (rel-exp (a ops bs asig)
               (cases resto-asig asig
                 (sin-asignacion ()
                                 (evaluar-nivel-rel (evaluar-aritmetica a env) ops bs env))
                 (con-asignacion (rhs)
                                 (if (null? ops)
                                     (let* ((id (identificador-de a))
                                            (v (evaluar-expresion rhs env)))
                                       (asignar-variable id v env)
                                       v)
                                     (error-lado-izquierdo))))))))

;;identificador-de
;;Si la expresión aritmética es solo un identificador (sin operadores ni llamadas),
;;retorna ese identificador; en otro caso lanza un error.

(define identificador-de
  (lambda (a)
    (cases aritmetica a
      (ad-exp (t ops ts)
              (if (null? ops)
                  (cases termino t
                    (mul-exp (f ops2 fs)
                             (if (null? ops2)
                                 (cases factor f
                                   (id-exp (id resto)
                                           (cases resto-id resto
                                             (resto-llamadas (argss)
                                                             (if (null? argss)
                                                                 id
                                                                 (error-lado-izquierdo)))))
                                   (else (error-lado-izquierdo)))
                                 (error-lado-izquierdo))))
                  (error-lado-izquierdo))))))


;;error-lado-izquierdo
;;Lanza el error cuando el lado izquierdo de -> no es una variable.

(define error-lado-izquierdo
  (lambda ()
    (eopl:error 'asignacion "El lado izquierdo de -> debe ser una variable")))


;;evaluar-nivel-rel
;;Recibe el valor acumulado, los operadores relacionales y las expresiones aritméticas
;;que faltan; aplica cada operador de izquierda a derecha y retorna el resultado.

(define evaluar-nivel-rel
  (lambda (acc ops bs env)
    (if (null? ops)
        acc
        (evaluar-nivel-rel (aplicar-op-rel (car ops) acc (evaluar-aritmetica (car bs) env))
                           (cdr ops) (cdr bs) env))))

;;evaluar-aritmetica
;;Evalúa sumas, restas y concatenaciones de izquierda a derecha.

(define evaluar-aritmetica
  (lambda (a env)
    (cases aritmetica a
      (ad-exp (t ops ts)
              (evaluar-nivel-ad (evaluar-termino t env) ops ts env)))))

;;evaluar-nivel-ad
;;Igual que evaluar-nivel-rel pero para +, - y concat sobre términos.

(define evaluar-nivel-ad
  (lambda (acc ops ts env)
    (if (null? ops)
        acc
        (evaluar-nivel-ad (aplicar-op-ad (car ops) acc (evaluar-termino (car ts) env))
                          (cdr ops) (cdr ts) env))))

;;evaluar-termino
;;Evalúa multiplicaciones y divisiones de izquierda a derecha.

(define evaluar-termino
  (lambda (t env)
    (cases termino t
      (mul-exp (f ops fs)
               (evaluar-nivel-mul (evaluar-factor f env) ops fs env)))))


;;evaluar-nivel-mul
;;Igual que evaluar-nivel-rel pero para * y / sobre factores.

(define evaluar-nivel-mul
  (lambda (acc ops fs env)
    (if (null? ops)
        acc
        (evaluar-nivel-mul (aplicar-op-mul (car ops) acc (evaluar-factor (car fs) env))
                           (cdr ops) (cdr fs) env))))

;;evaluar-factor
;;Evalúa literales, booleanos, paréntesis, primitivas unarias, print e identificadores
;;(variable o llamada a procedimiento).

(define evaluar-factor
  (lambda (f env)
    (cases factor f
      (numero-lit (num) num)
      (cadena-lit (txt) (substring txt 1 (- (string-length txt) 1)))
      (true-exp () #t)
      (false-exp () #f)
      (parentesis-exp (exp) (evaluar-expresion exp env))
      (primapp-un-exp (prim-unaria exp)
                      (aplicar-primitiva-unaria prim-unaria (evaluar-expresion exp env)))
      (print-exp (exps)
                 (begin (imprimir-valores (evaluar-lista-exp exps env))
                        0))
      (id-exp (id resto)
        (cases resto-id resto
          (resto-llamadas (argss)
                          (aplicar-llamadas (buscar-variable id env) id argss env))))
      ;; --- listas: casos aquí ---
      
      ;; --- vectores: casos aquí ---
      (vec-exp (vect) (vector-expresiones vect env))
      
      (make-vector-exp (num)
                    (if (and (integer? num) (>= num 0))
                        (make-vector num)
                        (eopl:error 'make-vector "El numero debe ser entero no negativo")
                           ))
      (ref-vec-exp (exp1 exp2)
                   (let ([vect (evaluar-expresion exp1 env)]
                         [num (evaluar-expresion exp2 env)])
                     (if (and(vector? vect)(integer? num))
                         (if (and(>= num 0)(< num (vector-length vect)))
                             (vector-ref vect num)
                             (eopl:error ref-vec-exp "El valor no es un indice valido")
                             )
                         (eopl:error ref-vec-exp "Parametros no permitidos: se esperaba un vector y un indice entero")
                         )
                     )
                   )
      (set-vec-exp(exp1 exp2 exp3)
                   (let* ([id (identificador-de exp1)]
                          [ref-tipo (buscar-info id env)]
                          [vec-tipo (cadr ref-tipo)]
                          [tipo (cases tipo vec-tipo
                                  (tipo-vector (t) t)
                                  (else(eopl:error 'set-vec-exp "La variable no es de tipo vector")))]

                          [vect (evaluar-expresion exp1 env)]
                          [val (evaluar-expresion exp2 env)]
                          [num (evaluar-expresion exp3 env)])
                     (if (and(vector? vect)(and(integer? num)))
                         (if (and(>= num 0)(< num (vector-length vect)))
                             (if (valor-coincide-tipo? tipo val)
                                 (vector-set! vect num val)
                                 (eopl:error 'set-vec-exp "El valor a remplazar es de distinto tipo al vector")
                                 )
                             (eopl:error 'set-vec-exp "El numero no es un indice valido")
                             )
                         (eopl:error 'ref-vec-exp "Parametros no permitidos: se esperaba un vector y un indice entero")
                         )
                     )
                   )
      
      ;; --- diccionarios: casos aquí ---
      
      ;; --- SAT: casos aquí ---
      )))

;;Funciones auxiliares

;;vector-expresiones
;;Recibe una lista de expresiones y devuelve el vector de sus resultados
;;Se usa para crear un vector

(define vector-expresiones
  (lambda (lst env)
    (let ([eval-listexp (lambda (l1) (evaluar-expresion l1 env))])

      (if (null? lst)
        (make-vector 0)
        (list->vector(map eval-listexp lst))
        )
      
      )
    )
  )

;;contar-elementos
;;Recibe una lista y devuelve cuántos elementos tiene.
;;Se usa para calcular la longitud de un texto (primitiva longitud)

(define contar-elementos
  (lambda (lst)
    (if (null? lst)
        0
        (+ 1 (contar-elementos (cdr lst))))))

;;unir-listas
;;Recibe dos listas y devuelve una sola con los elementos de la primera
;;seguidos de los de la segunda.
;;Se usa para concatenar textos (primitiva concat)

(define unir-listas
  (lambda (l1 l2)
    (if (null? l1)
        l2
        (cons (car l1)
              (unir-listas (cdr l1) l2)))))

;;aplicar-llamadas
;;Si no hay listas de argumentos, devuelve el valor de la variable tal cual.
;;Si las hay, aplica el procedimiento a cada lista en orden (normalmente es solo una).

(define aplicar-llamadas
  (lambda (valor id argss env)
    (if (null? argss)
        valor
        (if (procVal? valor)
            (aplicar-llamadas (aplicar-procedimiento valor (evaluar-lista-exp (car argss) env))
                              id
                              (cdr argss)
                              env)
            (eopl:error 'app-exp "~s no es un procedimiento" id)))))

;;aplicar-op-rel
;;Aplica un operador relacional a dos valores ya evaluados; retorna un booleano.

(define aplicar-op-rel
  (lambda (prim v1 v2)
    (cases op-rel prim
      (primitiva-mayor () (> v1 v2))
      (primitiva-menor () (< v1 v2))
      (primitiva-mayor-igual () (>= v1 v2))
      (primitiva-menor-igual () (<= v1 v2))
      (primitiva-diferente () (not (iguales? v1 v2)))
      (primitiva-comparador-igual () (iguales? v1 v2)))))

;;aplicar-op-ad
;;Aplica suma, resta o concatenación a dos valores ya evaluados.

(define aplicar-op-ad
  (lambda (prim v1 v2)
    (cases op-ad prim
      (primitiva-suma () (+ v1 v2))
      (primitiva-resta () (- v1 v2))
      (primitiva-concat () (list->string (unir-listas (string->list v1) (string->list v2)))))))

;;aplicar-op-mul
;;Aplica multiplicación o división a dos valores ya evaluados.

(define aplicar-op-mul
  (lambda (prim v1 v2)
    (cases op-mul prim
      (primitiva-multi () (* v1 v2))
      (primitiva-div () (/ v1 v2)))))

;;iguales?
;;Recibe dos valores y retorna #t si son iguales. Compara booleanos con eq? y
;;números con =. Se usa en las primitivas == y !=.

(define iguales?
  (lambda (v1 v2)
    (if (and (boolean? v1) (boolean? v2))
        (eq? v1 v2)
        (= v1 v2))))
                       

;;aplicar-primitiva-unaria
;;Recibe una primitiva unaria y un valor ya evaluado, y devuelve el resultado
;;de aplicarle la primitiva (longitud, add1, sub1, neg).
;;Se usa en evaluar-factor para las expresiones de la forma primitiva(exp)

(define aplicar-primitiva-unaria
  (lambda (prim v1)
    (cases primitiva-unaria prim
      (primitiva-longitud () (contar-elementos (string->list v1))) 
      (primitiva-add1 () (+ v1 1))
      (primitiva-sub1 () (- v1 1))
      (primitiva-negacion-booleana () (not (valor-verdad? v1)))
      (primitiva-vec? () (vector? v1))
     )))

;;valor-verdad?
;;Función que recibe un booleano o un numero y retorna #f si es 0 y #t en cualquier otro caso.

(define valor-verdad?
  (lambda (x)
    (cond
      ((boolean? x) x)
      ((number? x) (not (zero? x)))
      (else (eopl:error 'valor-verdad? "El valor ~s no se puede usar como condición" x)))))

;;evaluar-lista-exp
;;Funcion que recibe una lista de expresiones y un entorno donde opera retorna la
;;lista del resultado de evaluar cada expresion en el entorno dado

(define evaluar-lista-exp
  (lambda(exps env)
    (cond
      [(null? exps) '()]
      [else
       (cons (evaluar-expresion (car exps) env) (evaluar-lista-exp (cdr exps) env))]
      )
    )
  )


;;aplicar-procedimiento
;;Verifica cantidad y tipo de los argumentos y evalúa el cuerpo en un ambiente nuevo
;;que extiende el de la cerradura. El paso es por valor: cada parámetro es una
;;referencia nueva.

(define aplicar-procedimiento
  (lambda (proc args)
    (cases procVal proc
      (cerradura (tipos ids cuerpo amb)
                 (if (not (= (length ids) (length args)))
                     (eopl:error 'aplicar-procedimiento
                                 "Se esperaban ~s argumentos y se recibieron ~s"
                                 (length ids) (length args))
                     (evaluar-secuencia
                      cuerpo
                      (extender-ambiente-var ids
                                             (validar-args tipos ids args)
                                             tipos
                                             (map (lambda (i) #f) ids)
                                             amb)))))))


;;validar-args
;;Recibe la lista de tipos, de identificadores y de argumentos de un procedimiento,
;;valida cada argumento contra su tipo y retorna la lista de argumentos.

(define validar-args
  (lambda (tipos ids args)
    (if (null? tipos)
        '()
        (cons (validar-tipo (car tipos) (car ids) (car args))
              (validar-args (cdr tipos) (cdr ids) (cdr args))))))

;;evaluar-secuencia
;;Evalúa las sentencias de una secuencia en orden y retorna el valor de la última.

(define evaluar-secuencia
  (lambda (sec env)
    (cases secuencia sec
      (una-secuencia (s ss) (evaluar-sentencias (cons s ss) env)))))

;;evaluar-sentencias
;;Evalúa una lista de sentencias en orden y retorna el valor de la última.

(define evaluar-sentencias
  (lambda (sents env)
    (let ((v (evaluar-sentencia (car sents) env)))
      (if (null? (cdr sents))
          v
          (evaluar-sentencias (cdr sents) env)))))

;;evaluar-sentencia
;;Evalúa una sentencia: expresión simple, estructura de control o return.
;;return no interrumpe la ejecución; solo produce el valor de su expresión.

(define evaluar-sentencia
  (lambda (sent env)
    (cases sentencia sent
      (sentencia-exp (exp) (evaluar-expresion exp env))
      (sentencia-estructura (est) (evaluar-estructura est env))
      (sentencia-return (r) (evaluar-retorno r env)))))


;;evaluar-retorno
;;Evalúa lo que va después de return: una expresión o una estructura de control.

(define evaluar-retorno
  (lambda (r env)
    (cases retorno r
      (retorno-exp (exp) (evaluar-expresion exp env))
      (retorno-estructura (est) (evaluar-estructura est env)))))

;;evaluar-estructura
;;Evalúa BLOCK, LOCALS, if, switch, while y for.

(define evaluar-estructura
  (lambda (est env)
    (cases estructura est
      (block-est (sec) (evaluar-secuencia sec env))
      (locals-est (d ds sec)
                  (evaluar-secuencia sec (evaluar-declaraciones (cons d ds) env)))
      (if-est (test sec-v sec-f)
              (if (valor-verdad? (evaluar-expresion test env))
                  (evaluar-secuencia sec-v env)
                  (evaluar-secuencia sec-f env)))
      (switch-est (exp vals secs default)
                  (evaluar-switch (evaluar-expresion exp env) vals secs default env))
      (while-est (test sec) (evaluar-while test sec env))
      (for-est (init test update sec)
               (begin (evaluar-expresion init env)
                      (evaluar-for test update sec env))))))

;;evaluar-switch
;;Ejecuta la secuencia del primer case cuyo valor coincide; si ninguno, el default.

(define evaluar-switch
  (lambda (v vals secs default env)
    (cond
      ((null? vals) (evaluar-secuencia default env))
      ((valores-iguales? v (evaluar-expresion (car vals) env))
       (evaluar-secuencia (car secs) env))
      (else (evaluar-switch v (cdr vals) (cdr secs) default env)))))


;;valores-iguales?
;;Compara dos valores para el switch: números con = y los demás (cadenas, booleanos) con equal?.

(define valores-iguales?
  (lambda (a b)
    (if (and (number? a) (number? b))
        (= a b)
        (equal? a b))))

;;evaluar-while
;;Repite la secuencia mientras la condición sea verdadera. Retorna 0.

(define evaluar-while
  (lambda (test sec env)
    (if (valor-verdad? (evaluar-expresion test env))
        (begin (evaluar-secuencia sec env)
               (evaluar-while test sec env))
        0)))

;;evaluar-for
;;Evalúa condición, cuerpo y actualización hasta que la condición sea falsa. Retorna 0.

(define evaluar-for
  (lambda (test update sec env)
    (if (valor-verdad? (evaluar-expresion test env))
        (begin (evaluar-secuencia sec env)
               (evaluar-expresion update env)
               (evaluar-for test update sec env))
        0)))

;;imprimir-valores
;;Imprime los valores separados por espacio y termina con salto de línea.

(define imprimir-valores
  (lambda (vals)
    (if (null? vals)
        (newline)
        (begin
          (display (formatear-valor (car vals)))
          (if (null? (cdr vals)) (newline) (display " "))
          (imprimir-valores (cdr vals))))))


;;formatear-valor
;;Convierte un valor a texto para print: True/False, listas (1 2 3), vectores [1, 2],
;;diccionarios {"a": 1}, instancias SAT y procedimientos.

(define formatear-valor
  (lambda (v)
    (cond
      ((eq? v #t) "True")
      ((eq? v #f) "False")
      ((number? v) (number->string v))
      ((string? v) v)
      ((symbol? v) (symbol->string v))
      ((null? v) "()")
      ((list? v) (string-append "(" (unir-con " " (map formatear-elemento v)) ")"))
      ((vector-lf? v) (string-append "[" (unir-con ", " (map formatear-elemento (contenido-vector v))) "]"))
      ((dict-lf? v) (string-append "{" (unir-con ", " (map formatear-par (pares-dict v))) "}"))
      ((instancia-sat? v) (formatear-sat v))
      ((procVal? v) "<procedimiento>")
      (else v))))

(define formatear-elemento
  (lambda (v)
    (if (string? v) (string-append "\"" v "\"") (formatear-valor v))))

(define formatear-par
  (lambda (p)
    (string-append (formatear-elemento (car p)) ": " (formatear-elemento (cdr p)))))

(define formatear-sat
  (lambda (v)
    (cases instancia-sat v
      (una-instancia (n cs)
                     (string-append "FNC " (number->string n) " ("
                                    (unir-con " and "
                                              (map (lambda (c)
                                                     (string-append "(" (unir-con " or " (map number->string c)) ")"))
                                                   cs))
                                    ")")))))

(define unir-con
  (lambda (sep strs)
    (cond ((null? strs) "")
          ((null? (cdr strs)) (car strs))
          (else (string-append (car strs) sep (unir-con sep (cdr strs)))))))

;(interpretador)