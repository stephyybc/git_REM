import ply.lex as lex
import ply.yacc as yacc
#import sys

codigoPrueba = open('TEST.txt','r')
#LEXICO
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
    'DO_WHILE',
    'FOR',
    'BREAK',
    'VECTOR',
    'MATRIX',
    'STARTO', #Palabra que inicia funcion principal
    'END' #Palabra que termina funcion
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


def t_ID(t):
    r'[a-zA-Z][a-zA-Z_0-9]*'
    if t.value.upper() in reserved: #revisar que está en el arreglo
        t.value = t.value.upper()
        t.type = t.value.upper()
    else:
            t.type = 'ID'
    return t

def t_NOMBRE(t):
    r'[a-zA-Z][a-zA-Z_0-9]*'
    if t.value.upper() in reserved: #revisar que está en el arreglo
        t.value = t.value.upper()
        t.type = t.value.upper()
    else:
            t.type = 'NOMBRE'
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

#Análsisi de sintaxis
#DIAGRAMA DE SINTAXIS
#precedence nos ayuda a jerarquizar las operaciones con mayor prioridad
#entre más abajo mayor prioridad
precedence = (
    ('nonassoc', 'GTTHAN', 'LESSTHAN', 'GTEQ', 'LOEQ'), 
    ('left','AND','OR'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'MULTIPLY', 'DIVIDE'),
    #('right', 'UMINUS'),            # Unary minus operator
)

#ahora analizamos las funciones

#Primero la funcion principal, estructura del programa
def p_main(p):
    '''
    main : ID STARTO PARLEFT PARRIGHT DOTS body ID END
    '''

#vamos a declarar body
#jerarquia a la izquierda
def p_body(p):
    '''
    body : body create_var 
         | empty
    '''
def p_createVar(p):
    '''
    create_var : ID DOTS type DOTSEQ value ENDING
    '''

def p_type(p):
    '''
    type : INT 
         | FLOAT
         | STRING     
    '''

def p_value(p):
    '''
    value : INT_VALUE 
          | FLOAT_VALUE
          | STRING_VALUE 
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

