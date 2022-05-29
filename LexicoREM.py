import ply.lex as lex
import ply.yacc as yacc
#import sys

codigoPrueba = open('TEST.txt','r')

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
    'ID',
    'ENDING', #semicolon
    'PLUSPLUS',
    'DOTSEQ' #dos puntos igual (asignacion de valor)
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

def t_STRING(t):
     r'"([^"\n]|(\\"))*"' #Puede ser cualquiera de esos caracteres entre comillas
     return t

def t_FLOAT(t): 
    r'\d+\.\d+'
    t.value = float(t.value)
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

def t_INT(t):
    r'\d+' #recibe numero decimales
    t.value = int(t.value) #accede al value y lo convierte a entero
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