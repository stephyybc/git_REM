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
    'CALL', #para lamada de funciones
    'TO' #cambio para el for


]

#Para unir tokens y values
tokens += reserved  

#Descripcion de tokens
t_PLUS = r'\+'
t_MINUS = r'\-'
t_MULTIPLY = r'\*'
t_DIVIDE = r'\/'
t_SAME = r'\=\='
t_GTEQ = r'>='
t_LOEQ = r'<='
t_GTTHAN = r'>'
t_LESSTHAN = r'<'
t_NOTEQUAL = r'!='
t_EQUAL = r'\='
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

def t_TO(t):
    r'TO'
    t.type = "TO"
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
#lexer.input(codigoPrueba.read()) #lectura del programa
#while True:
 #   tokenss = lexer.token() #Cuando se acaba el programa la variable vale none
  #  print(tokenss)
   # if not tokenss: 
    #    break


###################################
##### Análsisi de sintaxis ########
###################################

########################################
### DECLARAMOS LA TABLA DE SIMBOLOS ####
########################################

tabla_simbolos = {}
posicion = 0

#########################################
#### INICIAMOS CON LOS CUADRUPLOS #######
#########################################

pilas_operandos = [] #vamos a almacenar aqui 
temporales = ['T17', 'T16','T15','T14','T13','T12','T11','T10','T9','T8','T7','T6', 'T5','T4','T3','T2','T1','T0'] #Vamos declarando las temporales que sean necesarias
#temporales = [x for x in range (100,-1,-1)]
Lista_cuadruplos = [] #Lista donde se almacenaran los cuadruplos 
ContaCuadruplos = 0
Used_temporales = []

#############################
#### CUADRUPLOS EN CICLOS ###
#############################
pila_saltos = [] #donde se van a almacenar la direccion de donde vas a saltar

#############################
#### EXECUTION PROGRAM ######
#############################
#Program_counter = 0 #contador que recorre el programa
operacion = 0
operando1 = 0
operando2= 0
resultado = 0
Program_counter = 0
resultado_temporales = [None,None,None,None,None,None,None,None,None,None,None]

def getCuadInfo(): #funcion para obtener los parametros del cuadruplo 
    global operacion
    global operando1
    global operando2
    global resultado

    #Donde localizamos cada elemento dentro del cuadruplo
    global Program_counter
    cuadruplo_actual = Lista_cuadruplos[Program_counter]
    operacion = Lista_cuadruplos[Program_counter][0]
    operando1 = Lista_cuadruplos[Program_counter][1]
    operando2 = Lista_cuadruplos[Program_counter][2]
    resultado = Lista_cuadruplos[Program_counter][3]
    #print ("Cuadrplo 0 ->",Lista_cuadruplos[0])
    #print("Cuadrplo ->", cuadruplo_actual)
    #print("TABLA",tabla_simbolos)

def Ejecucion():

    global operacion
    global operando1
    global operando2
    global resultado
    global Program_counter
    print("A VER TEMPORALES",temporales)
    print("TEMPORALES UTILZADOS",Used_temporales)

    print("ERROR WHILE",len(Lista_cuadruplos))
    print("ERROR WHILE",Program_counter)
    print("TERMINAL : ")
    while Program_counter < len(Lista_cuadruplos):
        getCuadInfo()
        #Verifica operandoss
        if operando1 in tabla_simbolos:
            if("value" in tabla_simbolos[operando1]):
                operando1 = tabla_simbolos[operando1]["value"]

        elif operando1 in Used_temporales:
            operando1 = resultado_temporales[int(operando1[1])]

        #Verifica operando 2
        if operando2 in tabla_simbolos:
            if("value" in tabla_simbolos[operando2]):
                operando2 = tabla_simbolos[operando2]["value"]

        elif operando2 in Used_temporales:
            operando2 = resultado_temporales[resultado]  
    
        if(resultado in Used_temporales):
            resultado_index = resultado[1]
            #Operaciones aritméticas}
            if operacion == ":=": 
                # print("resultadosss",resultado)
                # print("resultados temporales", resultado_temporales)
                # print("operando 1",operando1)
                resultado_temporales[resultado] = operando1
                Program_counter += 1

            elif operacion == "+":
                # print("resultadosss",resultado)
                # print("resultados temporales", resultado_temporales)
                # print("operando 1",operando1)
                resultado_temporales[int(resultado[1])] = (operando1 + operando2)
                Program_counter += 1

            elif operacion == "-":
                resultado_temporales[resultado] = (operando1 - operando2)
                Program_counter += 1
            
            elif operacion == "*":
                resultado_temporales[resultado] = (operando1 * operando2)
                Program_counter += 1
            
            elif operacion == "/":
                resultado_temporales[resultado] = (operando1 / operando2)
                Program_counter += 1  
            
            #operaciones lógicas
            elif operacion == "==":
                resultado_temporales[resultado] = (operando1 == operando2)
                Program_counter += 1

            elif operacion == ">":
                resultado_index = resultado[1]
                resultado_temporales[int(resultado_index)] = (operando1 > operando2)
                Program_counter += 1

            elif operacion == "<":
                resultado_index = resultado[1]
                resultado_temporales[int(resultado_index)] = (operando1 < operando2)
                Program_counter += 1

            elif operacion == ">=":
                resultado_temporales[int(resultado_index)] = (operando1 >= operando2)
                Program_counter += 1

            elif operacion == "<=":
                resultado_temporales[int(resultado[1])] = (operando1 <= operando2)
                Program_counter += 1

            elif operacion == "&":
                resultado_temporales[resultado] = (operando1 and operando2)
                Program_counter += 1

            elif operacion == "|":
                resultado_temporales[resultado] = (operando1 or operando2)
                Program_counter += 1

        else:
            if operacion == ":=":
                if (operando2 != None):
                    if resultado in tabla_simbolos : #Si esta ya fue declarada
                        sys.exit(f"La variable {resultado} ya fue declarada")
                    else: 
                        tabla_simbolos[resultado] = {} #Creo un diccionario dentro
                        tabla_simbolos[resultado]["Index"] = Simbol_Index 
                        tabla_simbolos[resultado]["Type"]  = operando2
                        tabla_simbolos[resultado]["value"] = operando1
                        Simbol_Index = Simbol_Index + 1
                        Program_counter += 1
                else:
                        tabla_simbolos[resultado]["value"] = operando1
                        Program_counter += 1

            elif operacion == "+":
                tabla_simbolos[resultado]["value"] = operando1 + operando2
                Program_counter = Program_counter + 1
            
            elif operacion == "GOTO":
                Program_counter = resultado

            elif operacion == "GOTOF":
                if(operando1 == False):
                    Program_counter = resultado
                else:
                    Program_counter += 1

            elif operacion == "CALL":
                Program_counter = resultado
            
            elif operacion == "PRINT":
                print(operando1)
                Program_counter = Program_counter+1
            
            elif operacion == "READ":
                Program_counter = Program_counter+1
            else:
                Program_counter+1

            
    print("TABLA DE SIMBOLOS FINAL",tabla_simbolos)
    print("FINAL DE TERMINAL :)")
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
    tabla_simbolos[name][" memoryIndex"] = posicion
    tabla_simbolos[name]["type"] = tipo
    tabla_simbolos[name]["value"] = 0
    posicion = posicion + 1

def p_call(p):
    '''
    call : CALL ID ENDING 
    '''
    if not p[2] in tabla_simbolos:
        print("La variable llamada no está en tabla de simbolos")
        sys.exit()

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
         | body condicion ENDING
         | empty
    '''
#Creacion de variables
def p_createVar(p):
    '''
    createVar : ID DOTS type DOTSEQ aritmeticExpression ENDING
    '''
    p[0] = p[1]
    # Tabla de simbolos variables
    global posicion
    global ContaCuadruplos
    valor = None
    name = p[1]
    tipo = p[3]
    tabla_simbolos[name] = {} #Lista
    tabla_simbolos[name][" memory index = "] = posicion
    tabla_simbolos[name]["type = "] = tipo
    tabla_simbolos[name]["value"] = valor
    posicion = posicion + 1

    print(p[1])

    if p[1] in tabla_simbolos:
        #Cuadruplos variables
        print(pilas_operandos)
        operando1 = pilas_operandos.pop()
        print(pilas_operandos)
        cuadruplo = [':=',operando1,None,p[1]]
        print("Variable: ",cuadruplo)
        Lista_cuadruplos.append(cuadruplo)
        ContaCuadruplos = ContaCuadruplos +1
    else:
        sys.exit("Esta variable completa no está declarada")

def p_createVarSimp(p):
    '''
    createVar : ID DOTS type ENDING
    ''' 
    p[0] = p[1]
    # Tabla de simbolos variables
    name = p[1]
    global posicion
    tipo = p[3]
    tabla_simbolos[name] = {} #Lista
    tabla_simbolos[name][" memory index = "] = posicion
    tabla_simbolos[name]["type = "] = tipo
    tabla_simbolos[name]["value"] = None
    posicion = posicion + 1 
    
    if p[1] in tabla_simbolos:
        #Cuadruplos variables
        global ContaCuadruplos
        print(pilas_operandos)
        cuadruplo = [':=',None,None,p[1]]
        print("Variable: ",cuadruplo)
        Lista_cuadruplos.append(cuadruplo)
        ContaCuadruplos = ContaCuadruplos +1
    else:
        sys.exit("Variable simple no esta declarada")

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
    cuadruplo = [':=',operando1,None,p[1]]
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

##############################
## DECLARACION DE ESTATUTOS ##
##############################

#Declaracion del estatuto if
def p_if(p):
    '''
    estatuto_if : inicio_if body final_if body END IF ENDING
                | inicio_if body END IF ENDING
    '''
    print("Pila de saltos", pila_saltos)
    Lista_cuadruplos[pila_saltos.pop()][3] = ContaCuadruplos
    modify_Index = pila_saltos.pop()
    Lista_cuadruplos[modify_Index][3] = ContaCuadruplos - 1 

def p_if_gotof(p):
    '''
    inicio_if : IF condicion THEN DOTS
    '''
    global ContaCuadruplos
    resultado = pilas_operandos.pop()
    print(resultado)
    cuadruplo = ["GOTOF",resultado,None,None]
    Lista_cuadruplos.append(cuadruplo)
    pila_saltos.append(ContaCuadruplos)
    ContaCuadruplos = ContaCuadruplos +1

def p_if_goto(p):
    '''
    final_if :  IF_NOT THEN DOTS
    '''
    global ContaCuadruplos
    cuadruplo = ["GOTO",None,None,None]
    Lista_cuadruplos.append(cuadruplo)
    #Used_temporales.append(resultado) #que se guarden los cuadruplos en la lista de temporales usados
    pila_saltos.append(ContaCuadruplos)
    ContaCuadruplos = ContaCuadruplos +1

#for tiene el limite superior y el limite inferior 
def p__for(p):
    '''
    estatuto_for : inicio_for body END LOOP ENDING
    '''
    global ContaCuadruplos
    print("PILA DE SALTOS",pila_saltos)
    operando1 = pilas_operandos.pop()
    resultado = temporales.pop()
    cuadruplo = ['+',operando1,1,operando1]
    Lista_cuadruplos.append(cuadruplo)
    Used_temporales.append(resultado) #que se guarden los cuadruplos en la lista de temporales usados
    print("RESULTADO DEL FOR",resultado)
    ContaCuadruplos = ContaCuadruplos + 1
    print(cuadruplo)
    cuadruplo = ["GOTO",None,None,None]
    Lista_cuadruplos.append(cuadruplo)
    pila_saltos.append(ContaCuadruplos)
    ContaCuadruplos = ContaCuadruplos +1
    modify_Index = pila_saltos.pop()
    Lista_cuadruplos[modify_Index][3] = ContaCuadruplos - modify_Index + 2
    modify_Index = pila_saltos.pop()
    Lista_cuadruplos[modify_Index][3] = ContaCuadruplos

def p_forGotof(p):
    '''
    inicio_for : FOR ID TO forValues LOOP DOTS
    '''
    global ContaCuadruplos
    pilas_operandos.append(p[2])
    pilas_operandos.append(p[4])
    operando2 = pilas_operandos.pop()
    operando1 = pilas_operandos.pop()
    print("ESTE ES EL OPERANDO 2:",operando2)
    print("ESTE ES EL OPERANDO 1:",operando1)
    resultado = temporales.pop()
    print("ESTE ES EL RESULTADO",resultado)
    cuadruplo = ['<=',operando1,operando2,resultado]
    Lista_cuadruplos.append(cuadruplo)
    pila_saltos.append(ContaCuadruplos)
    print("CUADRUPLO DEL FOR",cuadruplo)
    ContaCuadruplos = ContaCuadruplos +1
    #cuadruplo del gotof
    cuadruplo = ["GOTOF",resultado,None,None]
    Lista_cuadruplos.append(cuadruplo)
    Used_temporales.append(resultado) #que se guarden los cuadruplos en la lista de temporales usados
    print("RESULTADO DEL FOR INICIAL",resultado)
    pila_saltos.append(ContaCuadruplos)
    ContaCuadruplos = ContaCuadruplos +1
    print(cuadruplo)
    print(pilas_operandos)
    pilas_operandos.append(operando1)

    

#Declaracion del estatuto while y sus cuadruplos
def p_while(p):
    '''
    estatuto_while : inicio_while body END WHILE ENDING
    '''
    global ContaCuadruplos
    salto = pila_saltos.pop()
    Lista_cuadruplos[salto][3] = ContaCuadruplos +1
    cuadruplo = ["GOTO",None,None,salto -1]
    Lista_cuadruplos.append(cuadruplo)
    #Used_temporales.append(resultado) #que se guarden los cuadruplos en la lista de temporales usados
    print("RESULTADO DEL WHILE",resultado)
    pila_saltos.append(ContaCuadruplos -1)
    ContaCuadruplos = ContaCuadruplos +1

def p_whileGotof(p):
    '''
    inicio_while : WHILE condicion DOTS
    '''
    global ContaCuadruplos
    resultado = pilas_operandos.pop()
    print (resultado)
    cuadruplo = ["GOTOF",resultado,None,None]
    print("CUADRUPLO DEL WHILE",cuadruplo)
    Lista_cuadruplos.append(cuadruplo)
    pila_saltos.append(ContaCuadruplos)
    ContaCuadruplos = ContaCuadruplos +1

#Declaracion estatuto print
def p_print(p): 
    '''
    print : PRINT PARLEFT printValues PARRIGHT ENDING
    '''
    global ContaCuadruplos
    pilas_operandos.append(p[3])
    operando1 = pilas_operandos.pop()
    print("OPERANDO 1 PRINT",operando1)
    cuadruplo = ['PRINT',operando1,None,None]
    Lista_cuadruplos.append(cuadruplo)
    ContaCuadruplos = ContaCuadruplos +1
    
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
    Used_temporales.append(resultado) #que se guarden los cuadruplos en la lista de temporales usados
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
    Used_temporales.append(resultado) #que se guarden los cuadruplos en la lista de temporales usados
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
    Used_temporales.append(resultado) #que se guarden los cuadruplos en la lista de temporales usados
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
    Used_temporales.append(resultado) #que se guarden los cuadruplos en la lista de temporales usados
    ContaCuadruplos = ContaCuadruplos +1


##########################
#EXPRESIONES COMPARATIVAS#
##########################

def p_exGTEQUAL(p):
    '''
    condicion : condicion GTEQ condicion
    '''
    global ContaCuadruplos
    print(pilas_operandos)
    operando2 = pilas_operandos.pop()
    print(pilas_operandos)
    operando1 = pilas_operandos.pop()
    print(pilas_operandos)
    resultado = temporales.pop()
    pilas_operandos.append(resultado)
    cuadruplo = ['>=',operando1,operando2,resultado]
    print("GREATER EQUAL: ",cuadruplo)
    Lista_cuadruplos.append(cuadruplo)
    Used_temporales.append(resultado) #que se guarden los cuadruplos en la lista de temporales usados
    ContaCuadruplos = ContaCuadruplos +1


def p_exLOEQUAL(p):
    '''
    condicion : condicion LOEQ condicion
    '''
    global ContaCuadruplos
    print(pilas_operandos)
    operando2 = pilas_operandos.pop()
    print(pilas_operandos)
    operando1 = pilas_operandos.pop()
    print(pilas_operandos)
    resultado = temporales.pop()
    pilas_operandos.append(resultado)
    cuadruplo = ['<=',operando1,operando2,resultado]
    print("LOWER EQUAL: ",cuadruplo)
    Lista_cuadruplos.append(cuadruplo)
    Used_temporales.append(resultado) #que se guarden los cuadruplos en la lista de temporales usados
    ContaCuadruplos = ContaCuadruplos +1

def p_exGTTHAN(p):
    '''
    condicion : condicion GTTHAN condicion
    '''
    global ContaCuadruplos
    print(pilas_operandos)
    operando2 = pilas_operandos.pop()
    print(pilas_operandos)
    operando1 = pilas_operandos.pop()
    print(pilas_operandos)
    resultado = temporales.pop()
    pilas_operandos.append(resultado)
    cuadruplo = ['>',operando1,operando2,resultado]
    print("GREATER THAN: ",cuadruplo)
    Lista_cuadruplos.append(cuadruplo)
    Used_temporales.append(resultado) #que se guarden los cuadruplos en la lista de temporales usados
    ContaCuadruplos = ContaCuadruplos +1

def p_exLESSTHAN(p):
    '''
    condicion : condicion LESSTHAN condicion
    '''
    global ContaCuadruplos
    print(pilas_operandos)
    operando2 = pilas_operandos.pop()
    print(pilas_operandos)
    operando1 = pilas_operandos.pop()
    print(pilas_operandos)
    resultado = temporales.pop()
    pilas_operandos.append(resultado)
    cuadruplo = ['<',operando1,operando2,resultado]
    print("LESS THAN: ",cuadruplo)
    Lista_cuadruplos.append(cuadruplo)
    Used_temporales.append(resultado) #que se guarden los cuadruplos en la lista de temporales usados
    ContaCuadruplos = ContaCuadruplos +1

def p_exNOTEQUAL(p):
    '''
    condicion : condicion NOTEQUAL condicion
    '''
    global ContaCuadruplos
    print(pilas_operandos)
    operando2 = pilas_operandos.pop()
    print(pilas_operandos)
    operando1 = pilas_operandos.pop()
    print(pilas_operandos)
    resultado = temporales.pop()
    pilas_operandos.append(resultado)
    cuadruplo = ['!=',operando1,operando2,resultado]
    print("NOT EQUAL: ",cuadruplo)
    Lista_cuadruplos.append(cuadruplo)
    Used_temporales.append(resultado) #que se guarden los cuadruplos en la lista de temporales usados
    ContaCuadruplos = ContaCuadruplos +1

def p_exSAME(p):
    '''
    condicion : condicion SAME condicion
    '''
    global ContaCuadruplos
    print(pilas_operandos)
    operando2 = pilas_operandos.pop()
    print(pilas_operandos)
    operando1 = pilas_operandos.pop()
    print(pilas_operandos)
    resultado = temporales.pop()
    pilas_operandos.append(resultado)
    cuadruplo = ['==',operando1,operando2,resultado]
    print("SAME AS: ",cuadruplo)
    Lista_cuadruplos.append(cuadruplo)
    Used_temporales.append(resultado) #que se guarden los cuadruplos en la lista de temporales usados
    ContaCuadruplos = ContaCuadruplos +1  
    
def p_exAND(p):
    '''
    condicion : condicion AND condicion
    '''
    global ContaCuadruplos
    print(pilas_operandos)
    operando2 = pilas_operandos.pop()
    print(pilas_operandos)
    operando1 = pilas_operandos.pop()
    print(pilas_operandos)
    resultado = temporales.pop()
    pilas_operandos.append(resultado)
    cuadruplo = ['&&',operando1,operando2,resultado]
    print("AND: ",cuadruplo)
    Lista_cuadruplos.append(cuadruplo)
    Used_temporales.append(resultado) #que se guarden los cuadruplos en la lista de temporales usados
    ContaCuadruplos = ContaCuadruplos +1  

def p_exOR(p):
    '''
    condicion : condicion OR condicion
    ''' 
    global ContaCuadruplos
    print(pilas_operandos)
    operando2 = pilas_operandos.pop()
    print(pilas_operandos)
    operando1 = pilas_operandos.pop()
    print(pilas_operandos)
    resultado = temporales.pop()
    pilas_operandos.append(resultado)
    cuadruplo = ['||',operando1,operando2,resultado]
    print("OR: ",cuadruplo)
    Lista_cuadruplos.append(cuadruplo)
    Used_temporales.append(resultado) #que se guarden los cuadruplos en la lista de temporales usados
    ContaCuadruplos = ContaCuadruplos +1  

def p_DeceiverGROUP(p):
    '''
    condicion : PARLEFT condicion PARRIGHT
    '''
    p[0] = p[2]

def p_Deceiver(p):
    '''
    condicion : Deceiver
    '''
    p[0] = p[1]

def p_deceiverParGroup(p): 
    '''
    Deceiver : INT_VALUE
             | ID
    '''
    pilas_operandos.append( p[1])

#Un for acepta como valores valores enteros y un ID
def p_forValues(p):
    '''
    forValues : INT_VALUE
              | ID
    '''
    p[0] = p[1] #necesario sino no despliega nada

def p_printValues(p):
    '''
    printValues : STRING_VALUE
                | INT_VALUE
                | ID
    '''
    p[0] = p[1]

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
print("\nCuadruplos FINALES")
for x in Lista_cuadruplos:
    print(x)

Ejecucion()

#Vamnos a imprimir la tabla de simbolos
pprint.pprint(tabla_simbolos)
print ("Número de cuadruplos: ",ContaCuadruplos)
