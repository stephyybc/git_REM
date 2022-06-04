import ply.lex as lex
import ply.yacc as yacc
import pprint
from collections import deque
#import sys

codigoPrueba = open('TEST.txt','r')
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

############################
## DECLARAMOS CUADRUPLOS ###
############################
PilaDeOperandos = []
PilaDeSaltos = []
PilaDeCuadruplos = []
PilaDeVariables = []
availTemporales = deque(['1','2','3','4','5','6','7','8']) #No conviene ponerlos como enteros, porque se podría confundir
TemporalesOperandos = availTemporales.copy()
ListaDeTemporales = [None]*9
cont = 0
ID = ''
Tf_aux = [] #Necesito que esto sea una pila para que no se pierdan los temporales

def expresiones(simbolo, op1, op2):
    if op2 in TemporalesOperandos: #Para saber si algún temporal es un operando
        availTemporales.append(op2)
    if op1 in TemporalesOperandos:
        availTemporales.append(op1)

    res = availTemporales.popleft()
    PilaDeOperandos.append(res)

    Generar(simbolo, op1, op2, res)

def Generar(simbolo, op1, op2, res):
    global cont
    PilaDeCuadruplos.append([simbolo, op1, op2, res])
    cont = cont + 1

def Rellenar(D):
    global cont
    PilaDeCuadruplos[D][2] = cont #Va acceder al cuádruplo número D a su tercer parámetro

def Ejecucion():
    PC = 0
    global PilaDeVariables 
    Pos = 0 #Contador Variables
    while PC < len(PilaDeCuadruplos) :
        Cuadruplo = PilaDeCuadruplos[PC]
        checarVariable = False
        existeVariable = False
        opcode = Cuadruplo[0]
        op1 = Cuadruplo[1]
        op2 = Cuadruplo[2]
        res = Cuadruplo[3]

        if res in availTemporales:
            indice = int(res)

        if(not isinstance(op1, int) and not isinstance(op1, float) and op1 != "" and opcode != "write" and opcode != "input"):
            if op1 in availTemporales:
                indice = int(op1)
                op1 = ListaDeTemporales[indice]
            else:
                for x in range(len(PilaDeVariables)): #Para buscar el elemento en la vista
                    if op1 == PilaDeVariables[x][0]:
                        op1 = PilaDeVariables[x][3]
                        existeVariable = True
                        break
                if existeVariable == False :
                    sys.exit("No existe la variable")

        if(not isinstance(op2, int) and not isinstance(op2, float) and op2 != ""):
            if op2 in availTemporales:
                indice = int(op2)
                op2 = ListaDeTemporales[indice]
            else:
                existeVariable = False 
                for x in range(len(PilaDeVariables)): #Para buscar el elemento en la vista
                    if op2 == PilaDeVariables[x][0]:
                        op2 = PilaDeVariables[x][3]
                        existeVariable = True
                        break
                if existeVariable == False :
                    sys.exit("No existe la variable")
            
        if opcode == "*":
            ListaDeTemporales[indice] = op1 * op2
            PC = PC + 1
        elif opcode == "/":
            ListaDeTemporales[indice] = op1 / op2
            PC = PC + 1
        elif opcode == "+":
            if res in availTemporales: 
                ListaDeTemporales[indice] = op1 + op2
            else: #Este aplica para cuando se suma en loe For
                for x in range(len(PilaDeVariables)): #Para buscar el elemento en la vista
                    if res == PilaDeVariables[x][0]:
                        PilaDeVariables[x][3] = op1 + op2
                        break
            PC = PC + 1
        elif opcode == "-":
            ListaDeTemporales[indice] = op1 - op2
            PC = PC + 1
        elif opcode == ">=":
            ListaDeTemporales[indice] = op1 >= op2
            PC = PC + 1
        elif opcode == "<=":
            ListaDeTemporales[indice] = op1 <= op2
            PC = PC + 1
        elif opcode == ">":
            ListaDeTemporales[indice] = op1 > op2
            PC = PC + 1
        elif opcode == "<":
            ListaDeTemporales[indice] = op1 < op2
            PC = PC + 1
        elif opcode == "==":
            ListaDeTemporales[indice] = op1 == op2
            PC = PC + 1
        elif opcode == "!=":
            ListaDeTemporales[indice] = op1 != op2
            PC = PC + 1
        elif opcode == "and":
            ListaDeTemporales[indice] = op1 and op2
            PC = PC + 1
        elif opcode == "or":
            ListaDeTemporales[indice] = op1 or op2
            PC = PC + 1

        elif opcode == "=":
            if res not in availTemporales: 
                for x in range(len(PilaDeVariables)): #Para buscar el elemento en la vista
                    if res == PilaDeVariables[x][0]:
                        PilaDeVariables[x][3] = op1
                        if isinstance(op1, int):
                            PilaDeVariables[x][2] = "int"
                        else:
                            PilaDeVariables[x][2] = "float"
                        checarVariable = True
                        break
                if checarVariable == False:
                    if isinstance(op1, int):
                            tipo = "int"
                    else:
                            tipo = "float"
                    PilaDeVariables.append([res, Pos, tipo, op1])
                    Pos = Pos + 1
            else:
                ListaDeTemporales[indice] = op1
                
            PC = PC + 1
        elif opcode == "goto":
            PC = op2
        elif opcode == "gotof":
            if (op1 == False):
                PC = op2
            else:
                PC = PC + 1
        elif opcode == "write":
            if(isinstance(op1, str) and len(op1) > 0):
                simbolo = '"'
                op1 = op1.replace(simbolo, "") #Quitará las comillas dobles
                if(op1 == "\\n"):
                    print(op1, end = "\n")
                else:
                    print(op1, end = " ")
            else:
                print(op2, end = " ")
            PC = PC + 1
        elif opcode == "input":
            for x in range(len(PilaDeVariables)): #Para buscar el elemento en la vista
                    if res == PilaDeVariables[x][0]:
                        PilaDeVariables[x][3] = int(input(op1))
                        PC = PC + 1
                        break
                

###################################
##### Análsisi de sintaxis ########
###################################
#DIAGRAMA DE SINTAXIS

### DECLARAMOS LA TABLA DE SIMBOLOS ####
tabla_simbolos = {}
posicion = 0

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
         | empty
    '''
#Creacion de variables
def p_createVar(p):
    '''
    createVar : ID DOTS type DOTSEQ value ENDING
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
              | ID DOTS type DOTS dimension DOTSEQ RECLEFT value COMMA value RECRIGHT RECLEFT value COMMA value RECRIGHT ENDING 
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
    updateVar : ID DOTSEQ value ENDING
    '''
#Actualizar arreglos
def p_updateArr(p):
    '''
    updateArr : ID dimension DOTSEQ value ENDING         
    '''
#Se define si el arreglo es de una dimension o de dos
def p_dimension(p):
    '''
    dimension : RECLEFT value RECRIGHT 
              | RECLEFT value RECRIGHT RECLEFT value RECRIGHT 
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
def p_value(p):
    '''
    value : INT_VALUE 
          | FLOAT_VALUE
          | STRING_VALUE 
          | ID
          | ID aritmeticExpression ID
    '''
def p_aritmeticExpression(p):
    '''
    aritmeticExpression : MINUS
                        | PLUS
                        | MULTIPLY
                        | DIVIDE
    '''
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
              | packu  
    '''
def p_packu(p):
    '''
    packu : INT_VALUE
          | FLOAT_VALUE
          | ID
    '''
def p_empty(p):
    'empty : '
    pass

def p_error(p):
    print("ERROR FOUND!!")
    print(p)

sintaxis = yacc.yacc()
try:
    with open("TEST.txt",  encoding="utf8") as f:
        file = f.read()
    sintaxis.parse(file)
except EOFError:
    pass

#Vamnos a imprimir la tabla de simbolos
pprint.pprint(tabla_simbolos)