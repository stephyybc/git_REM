import ply.lex as lex
import ply.yacc as yacc
import pprint
import sys

codigoPrueba = open('PRUEBASCUADRUPLOS.txt','r')
##################################
###### ANALISIS DE LEXICO ########
##################################
#Con los tokens defino cada palabra de mi lenguaje
tokens = [
    'INT',
    'FLOAT',
    'PLUS',
    'MINUS',
    'DIVIDE',
    'MULTIPLY',
    'EQUAL',
    'PARLEFT', #paréntesis izquierdo
    'PARRIGHT', #paréntesis derecho
    'BRACLEFT', #llave izquierda
    'BRACRIGHT', #llave derecha
    'SAME',
    'GTEQ', #mayor igual
    'LOEQ', #menos igual
    'GTTHAN', #mayor
    'LESSTHAN', #menor
    'NOTEQUAL', 
    'DOTS',
    'COMMA',
    'COMMENT',
    'RECLEFT', #rectángulo izquierdo
    'RECRIGHT', #rectángulo derecho
    'AND', 
    'OR', 
    'QUOM', #comillas 
    'STRING',
    'ID', #especificamente para funciones
    'NOMBRE', #especificamente para variables y arreglos
    'ENDING', #semicolon
    'PLUSPLUS',
    'DOTSEQ', #dos puntos igual (asignacion de valor)
    'STRING_VALUE', #el valor de string
    'INT_VALUE', #el valor dentro del int
    'FLOAT_VALUE' #el valor del float
]

#Palabras reservadas de mi lenguaje
reserved = [
    'MAIN',
    'READ',
    'PRINT',
    'IF',
    'IF_NOT',
    'WHILE',
    'FOR',
    'BREAK',
    'VECTOR',
    'MATRIX',
    'STARTO', #Palabra que inicia funcion principal
    'END', #Palabra que termina funcion
    'THEN',
    'LOOP', #Palabra con la que inicia un ciclo
    'IN', #Simbolo para el loop
    'CALL'#para lamada de funciones


]

#Para unir tokens y values
tokens += reserved  

#Descripcion de tokens
t_PLUS = r'\+'
t_MINUS = r'\-'
t_MULTIPLY = r'\*'
t_DIVIDE = r'\/'
t_EQUAL = r'\=\='
t_GTEQ = r'>='
t_LOEQ = r'<='
t_GTTHAN = r'>'
t_LESSTHAN = r'<'
t_NOTEQUAL = r'!='
t_SAME = r'\='
t_AND = r'&&'
t_OR = r'\|\|'
t_PARLEFT = r'\('
t_PARRIGHT = r'\)'
t_BRACLEFT = r'\{'
t_BRACRIGHT = r'\}'
t_RECLEFT = r'\['
t_RECRIGHT = r'\]'
t_COMMA = r'\,'
t_QUOM = r'\"'
t_ENDING = r'\;'
t_DOTS = r'\:'
t_PLUSPLUS = r'\+'
t_DOTSEQ = r'\:\='
t_ignore = ' \t'
#t es un string

#definimos funciones
def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

def t_COMMENT(t):
       r'\//.*' 
       pass

#palabra declarada string
def t_STRING_VALUE(t):
     r'"([^"\n]|(\\"))*"' #Puede ser cualquiera de esos caracteres entre comillas
     return t

def t_INT_VALUE(t):
    r'\d+' #recibe numero decimales
    t.value = int(t.value) #accede al value y lo convierte a entero
    return t

def t_FLOAT_VALUE(t):
     r'\d+\.\d+'
     t.value = float(t.value)
     return t

#valor de string
def t_STRING(t):
     r'STRING'
     t.type = 'STRING'
     return t

#valor de int
def t_INT(t):
    r'INT'
    t.type = 'INT'
    return t

#valor de float
def t_FLOAT(t):
    r'FLOAT'
    t.type = 'FLOAT'
    return t

#Palabra que inicia la funcion principal
def t_STARTO(t): 
    r'STARTO'
    t.type = "STARTO"
    return t

#Palabra que termina la funcion
def t_END(t): 
    r'END'
    t.type = "END"
    return t

def t_THEN(t): 
    r'THEN'
    t.type = "THEN"
    return t

def t_LOOP(t): 
    r'LOOP'
    t.type = "LOOP"
    return t

def t_IN(t): 
    r'IN'
    t.type = "IN"
    return t
    
def t_CALL(t): 
    r'CALL'
    t.type = "CALL"
    return t

#Palabra donde se almacenan los numeros del rango del for
def t_RANGO(t):
    r'\d+' #recibe numero decimales
    t.rango = int(t.rango) #accede al value y lo convierte a entero
    return t

def t_ID(t):
    r'[a-zA-Z][a-zA-Z_0-9]*'
    if t.value.upper() in reserved: #revisar que está en el arreglo
        t.value = t.value.upper()
        t.type = t.value.upper()
    else:
            t.type = 'ID'
    return t

def t_error(t):
    print("Error de léxico") 
    t.lexer.skip(1) #Por si no encuentra ninguno regresa error
    

lexer = lex.lex()
lexer.input(codigoPrueba.read()) #lectura del programa
while True:
    tokenss = lexer.token() #Cuando se acaba el programa la variable vale none
    print(tokenss)
    if not tokenss: 
        break


###################################
##### Análsisi de sintaxis ########
###################################
#DIAGRAMA DE SINTAXIS

########################################
### DECLARAMOS LA TABLA DE SIMBOLOS ####
########################################

tabla_simbolos = {}
posicion = 0

#########################################
#### INICIAMOS CON LOS CUADRUPLOS #######
#########################################

pilas_operandos = [] #vamos a almacenar aqui 
temporales = ['T1', 'T2','T3','T4','T5','T6','T7','T8','T9','T10','T11','T12',] #Vamos declarando las temporales que sean necesarias
Lista_cuadruplos = [] #Lista donde se almacenaran los cuadruplos 
ContaCuadruplos = 0

#precedence nos ayuda a jerarquizar las operaciones con mayor prioridad
#entre más abajo mayor prioridad
precedence = (
    ('nonassoc', 'GTTHAN', 'LESSTHAN', 'GTEQ', 'LOEQ'), 
    ('left','AND','OR'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'MULTIPLY', 'DIVIDE'),
)


#Ahora analizamos las funciones
#Primero la funcion principal, estructura del programa
def p_principal(p):
    '''
    principal : principal main 
              | principal createFunc
              | empty
    '''

def p_main(p):
    '''
    main : ID STARTO PARLEFT PARRIGHT DOTS body ID END
    '''

#Funciones declaradas dentro del main
def p_function(p):
    '''
    createFunc : ID STARTO DOTS body ID END
    '''
    global posicion
    name = p[1]
    tipo = "funcion"
    tabla_simbolos[name] = {} #Arreglo
    tabla_simbolos[name][" memory index = "] = posicion
    tabla_simbolos[name]["type = "] = tipo
    posicion = posicion + 1

def p_call(p):
    '''
    call : CALL ID ENDING 
    '''
#vamos a declarar body
#jerarquia a la izquierda
def p_body(p):
    '''
    body : body createVar 
         | body createArr
         | body call
         | body updateVar
         | body estatuto_if
         | body estatuto_for
         | body estatuto_while
         | body print
         | body read
         | body updateArr
         | body aritmeticExpression ENDING
         | empty
    '''
#Creacion de variables
def p_createVar(p):
    '''
    createVar : ID DOTS type DOTSEQ aritmeticExpression ENDING
              | ID DOTS type ENDING
    '''
    name = p[1]
    global posicion
    if name in tabla_simbolos: 
        print(" {name} It already exists")
    else:
        tipo = p[3]
        tabla_simbolos[name] = {} #Lista
        tabla_simbolos[name][" memory index = "] = posicion
        tabla_simbolos[name]["type = "] = tipo
        posicion = posicion + 1
        
     
#Creacion de arreglos
def p_createArr(p):
    '''
    createArr : ID DOTS type DOTS dimension ENDING
              | ID DOTS type DOTS dimension DOTSEQ RECLEFT aritmeticExpression COMMA aritmeticExpression RECRIGHT RECLEFT aritmeticExpression COMMA aritmeticExpression RECRIGHT ENDING 
    '''
    name = p[1]
    global posicion 
    if name in tabla_simbolos: 
        print("\n {name} It already exists")
    else:
        tipo = p[3]
        tabla_simbolos[name] = {} #Lista
        tabla_simbolos[name][" memory index = "] = posicion
        tabla_simbolos[name]["type = "] = tipo
        posicion = posicion + 1
     
#Funciona para actualizar el valor de las variables
def p_upddateVar(p):
    '''
    updateVar : ID DOTSEQ aritmeticExpression ENDING
    '''
    global ContaCuadruplos
    print(pilas_operandos)
    operando1 = pilas_operandos.pop()
    print(pilas_operandos)
    cuadruplo = ['=',operando1,None,p[1]]
    print("EQUAL: ",cuadruplo)
    Lista_cuadruplos.append(cuadruplo)
    ContaCuadruplos = ContaCuadruplos +1

#Actualizar arreglos
def p_updateArr(p):
    '''
    updateArr : ID dimension DOTSEQ aritmeticExpression ENDING         
    '''
    pilas_operandos.append(p[4])
#Se define si el arreglo es de una dimension o de dos
def p_dimension(p):
    '''
    dimension : RECLEFT aritmeticExpression RECRIGHT 
              | RECLEFT aritmeticExpression RECRIGHT RECLEFT aritmeticExpression RECRIGHT 
    '''
#Declaracion de estatutos
def p_if(p):
    '''
    estatuto_if : IF condicion THEN DOTS body IF_NOT THEN DOTS body END IF ENDING
                | IF condicion THEN DOTS body END IF ENDING
    '''
#for tiene el limite superior y el limite inferior 
def p__for(p):
    '''
    estatuto_for : FOR ID IN forValues COMMA forValues LOOP DOTS body END LOOP ENDING
    '''
def p_while(p):
    '''
    estatuto_while : WHILE condicion DOTS body END WHILE ENDING
    '''
def p_print(p): 
    '''
    print : PRINT PARLEFT printValues COMMA ID PARRIGHT ENDING
          | PRINT PARLEFT printValues PARRIGHT ENDING 
          | PRINT PARLEFT ID PARRIGHT ENDING
    '''
def p_read(p):
    '''
    read : READ DOTS PARLEFT ID PARRIGHT ENDING END
    '''
#Definicion 
def p_type(p):
    '''
    type : INT 
         | FLOAT
         | STRING     
    '''
    p[0] = p[1]
#def p_value(p):
    #'''
    #value : INT_VALUE 
     #     | FLOAT_VALUE
      #    | STRING_VALUE 
       #   | ID
    #'''

#########################
#EXPRESIONES ARITMETICAS#
#########################

def p_Giver(p):
    '''
    aritmeticExpression : Giver
    '''
    p[0] = p[1]

def p_GiverValues(p):
    '''
    Giver : INT_VALUE
          | FLOAT_VALUE
          | ID
    '''
    pilas_operandos.append(p[1])

def p_giverGroup(p): 
    '''
    Giver : PARLEFT aritmeticExpression PARRIGHT
    '''
    p[0] = p[2]

def p_exMINUS(p):
    'aritmeticExpression : aritmeticExpression MINUS aritmeticExpression'
    global ContaCuadruplos
    print(pilas_operandos)
    operando2 = pilas_operandos.pop()
    print(pilas_operandos)
    operando1 = pilas_operandos.pop()
    print(pilas_operandos)
    resultado = temporales.pop()
    pilas_operandos.append(resultado)
    cuadruplo = ['-',operando1,operando2,resultado]
    print("MINUS: ",cuadruplo)
    Lista_cuadruplos.append(cuadruplo)
    ContaCuadruplos = ContaCuadruplos +1

def p_exMULTIPLY(p):
    'aritmeticExpression : aritmeticExpression MULTIPLY aritmeticExpression'
    global ContaCuadruplos
    print(pilas_operandos)
    operando2 = pilas_operandos.pop()
    print(pilas_operandos)
    operando1 = pilas_operandos.pop()
    print(pilas_operandos)
    resultado = temporales.pop()
    pilas_operandos.append(resultado)
    cuadruplo = ['*',operando1,operando2,resultado]
    print("MULTIPLY: ",cuadruplo)
    Lista_cuadruplos.append(cuadruplo)
    ContaCuadruplos = ContaCuadruplos +1

def p_exPLUS(p):
    'aritmeticExpression : aritmeticExpression PLUS aritmeticExpression'
    global ContaCuadruplos
    print(pilas_operandos)
    operando2 = pilas_operandos.pop()
    print(pilas_operandos)
    operando1 = pilas_operandos.pop()
    print(pilas_operandos)
    resultado = temporales.pop()
    pilas_operandos.append(resultado)
    cuadruplo = ['+',operando1,operando2,resultado]
    print("SUMA: ",cuadruplo)
    Lista_cuadruplos.append(cuadruplo)
    ContaCuadruplos = ContaCuadruplos +1

def p_exDIVIDE(p):
    'aritmeticExpression : aritmeticExpression DIVIDE aritmeticExpression'
    global ContaCuadruplos
    print(pilas_operandos)
    operando2 = pilas_operandos.pop()
    print(pilas_operandos)
    operando1 = pilas_operandos.pop()
    print(pilas_operandos)
    resultado = temporales.pop()
    pilas_operandos.append(resultado)
    cuadruplo = ['/',operando1,operando2,resultado]
    print("DIVIDE: ",cuadruplo)
    Lista_cuadruplos.append(cuadruplo)
    ContaCuadruplos = ContaCuadruplos +1


##########################
#EXPRESIONES COMPARATIVAS#
##########################


#Un for acepta como valores valores enteros y un ID
def p_forValues(p):
    '''
    forValues : INT_VALUE
              | ID
    '''
def p_printValues(p):
    '''
    printValues : STRING_VALUE
    '''
def p_condicion(p):
    '''
    condicion : condicion GTTHAN condicion
              | condicion LESSTHAN condicion
              | condicion GTEQ condicion
              | condicion LOEQ condicion 
              | condicion NOTEQUAL condicion
              | condicion EQUAL condicion
              | condicion AND condicion
              | condicion 
    '''
def p_packuCondicion(p):
    '''
    condicion : packu
    '''
    p[0] = p[1]
def p_packu(p): #Valor que posiblemente reciba en condiciones
    '''
    packu : INT_VALUE
          | FLOAT_VALUE
          | ID
    '''
    pilas_operandos.append(p[1])
def p_packuGroup(p): 
    '''
    packu : PARLEFT aritmeticExpression PARRIGHT
    '''
    p[0] = p[2]

def p_empty(p):
    'empty : '
    pass

def p_error(p):
    print("ERROR FOUND!!")
    print(p)

sintaxis = yacc.yacc()
try:
    with open("PRUEBASCUADRUPLOS.txt",  encoding="utf8") as f:
        file = f.read()
    sintaxis.parse(file)
except EOFError:
    pass

#Vamnos a imprimir la tabla de simbolos
pprint.pprint(tabla_simbolos)
print ("Número de cuadruplos: ",ContaCuadruplos)
for x in Lista_cuadruplos:
    print(x)