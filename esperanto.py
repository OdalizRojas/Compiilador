#######################################
# IMPORTS
#######################################

from strings_with_arrows import *

import string
import os
import math

#######################################
# CONSTANTS
#######################################

NUMEROS = '0123456789'
LETRAS = string.ascii_letters
LETRAS_NUMEROS = LETRAS + NUMEROS

#######################################
# ERRORS
#######################################

class ManejoErrores:
  def __init__(self, posicion_inicial, posicion_final, tipo_error, detalles):
    self.posicion_inicial = posicion_inicial
    self.posicion_final = posicion_final
    self.tipo_error = tipo_error
    self.detalles = detalles

  def como_string(self):
    resultado  = f'{self.tipo_error}: {self.detalles}\n'
    resultado += f'Archivo {self.posicion_inicial.nombre_archivo}, linea {self.posicion_inicial.ln + 1}'
    resultado += '\n\n' + string_with_arrows(self.posicion_inicial.texto_archivo, self.posicion_inicial, self.posicion_final)
    return resultado

class ErrorCaracterIlegal(ManejoErrores):
  def __init__(self, posicion_inicial, posicion_final, detalles):
    super().__init__(posicion_inicial, posicion_final, 'Caracter Ilegal', detalles)

class ErrorCaracterEsperado(ManejoErrores):
  def __init__(self, posicion_inicial, posicion_final, detalles):
    super().__init__(posicion_inicial, posicion_final, 'Se esperaba un caracter.', detalles)

class ErrorSintaxisInvalida(ManejoErrores):
  def __init__(self, posicion_inicial, posicion_final, detalles=''):
    super().__init__(posicion_inicial, posicion_final, 'Sintaxis Invalida', detalles)

class ErrorTiempoEjecucion(ManejoErrores):
  def __init__(self, posicion_inicial, posicion_final, detalles, contexto):
    super().__init__(posicion_inicial, posicion_final, 'Error Tiempo de Ejecucion', detalles)
    self.contexto = contexto

  def as_string(self):
    resultado  = self.generar_rastreo()
    resultado += f'{self.tipo_error}: {self.detalles}'
    resultado += '\n\n' + string_with_arrows(self.posicion_inicial.texto_archivo, self.posicion_inicial, self.posicion_final)
    return resultado

  def generar_rastreo(self):
    resultado = ''
    pos = self.posicion_inicial
    ctx = self.contexto

    while ctx:
      resultado = f'  File {pos.nombre_archivo}, line {str(pos.ln + 1)}, in {ctx.display_name}\n' + resultado
      pos = ctx.parent_entry_pos
      ctx = ctx.parent

    return 'Rastreo (la ultima llamada mas reciente):\n' + resultado

#######################################
# Posicion
#######################################

class Posicion:
  def __init__(self, idx, ln, col, nombre_archivo, texto_archivo):
    self.idx = idx
    self.ln = ln
    self.col = col
    self.nombre_archivo = nombre_archivo
    self.texto_archivo = texto_archivo

  def avanzar(self, caracter_actual=None):
    self.idx += 1
    self.col += 1

    if caracter_actual == '\n':
      self.ln += 1
      self.col = 0

    return self

  def copy(self):
    return Posicion(self.idx, self.ln, self.col, self.nombre_archivo, self.texto_archivo)

#######################################
# TOKENS
#######################################

TT_INT				= 'INT'
TT_FLOAT    	= 'FLOAT'
TT_STRING			= 'STRING'
TT_IDENTIFIER	= 'IDENTIFIER'
TT_KEYWORD		= 'KEYWORD'
TT_PLUS     	= 'PLUS'
TT_MINUS    	= 'MINUS'
TT_MUL      	= 'MUL'
TT_DIV      	= 'DIV'
TT_POW				= 'POW'
TT_EQ					= 'EQ'
TT_LPAREN   	= 'LPAREN'
TT_RPAREN   	= 'RPAREN'
TT_LSQUARE    = 'LSQUARE'
TT_RSQUARE    = 'RSQUARE'
TT_EE					= 'EE'
TT_NE					= 'NE'
TT_LT					= 'LT'
TT_GT					= 'GT'
TT_LTE				= 'LTE'
TT_GTE				= 'GTE'
TT_COMMA			= 'COMMA'
TT_ARROW			= 'ARROW'
TT_NEWLINE		= 'NEWLINE'
TT_EOF				= 'EOF'

KEYWORDS = [
  'VAR',
  'AND',
  'OR',
  'NOT',
  'SE',
  'ELIF',
  'ELSE',
  'POR',
  'AL',
  'STEP',
  'WHILE',
  'FUNKCIO',
  'DO',
  'FINI',
  'REVENO',
  'CONTINUE',
  'BREAK',
]

class Token:
  def __init__(self, type_, value=None, posicion_inicial=None, posicion_final=None):
    self.type = type_
    self.value = value

    if posicion_inicial:
      self.posicion_inicial = posicion_inicial.copy()
      self.posicion_final = posicion_inicial.copy()
      self.posicion_final.avanzar()

    if posicion_final:
      self.posicion_final = posicion_final.copy()

  def matches(self, type_, value):
    return self.type == type_ and self.value == value

  def __repr__(self):
    if self.value: return f'{self.type}:{self.value}'
    return f'{self.type}'

#######################################
# ANALIZADOR LEXICO
#######################################

class Lexer:
  def __init__(self, nombre_archivo, text):
    self.nombre_archivo = nombre_archivo
    self.text = text
    self.pos = Posicion(-1, 0, -1, nombre_archivo, text)
    self.caracter_actual = None
    self.avanzar()

  def avanzar(self):
    self.pos.avanzar(self.caracter_actual)
    self.caracter_actual = self.text[self.pos.idx] if self.pos.idx < len(self.text) else None

  def make_tokens(self):
    tokens = []

    while self.caracter_actual != None:
      if self.caracter_actual in ' \t':
        self.avanzar()
      elif self.caracter_actual == '#':
        self.skip_comment()
      elif self.caracter_actual in ';\n':
        tokens.append(Token(TT_NEWLINE, posicion_inicial=self.pos))
        self.avanzar()
      elif self.caracter_actual in NUMEROS:
        tokens.append(self.make_number())
      elif self.caracter_actual in LETRAS:
        tokens.append(self.make_identifier())
      elif self.caracter_actual == '"':
        tokens.append(self.make_string())
      elif self.caracter_actual == '+':
        tokens.append(Token(TT_PLUS, posicion_inicial=self.pos))
        self.avanzar()
      elif self.caracter_actual == '-':
        tokens.append(self.make_minus_or_arrow())
      elif self.caracter_actual == '*':
        tokens.append(Token(TT_MUL, posicion_inicial=self.pos))
        self.avanzar()
      elif self.caracter_actual == '/':
        tokens.append(Token(TT_DIV, posicion_inicial=self.pos))
        self.avanzar()
      elif self.caracter_actual == '^':
        tokens.append(Token(TT_POW, posicion_inicial=self.pos))
        self.avanzar()
      elif self.caracter_actual == '(':
        tokens.append(Token(TT_LPAREN, posicion_inicial=self.pos))
        self.avanzar()
      elif self.caracter_actual == ')':
        tokens.append(Token(TT_RPAREN, posicion_inicial=self.pos))
        self.avanzar()
      elif self.caracter_actual == '[':
        tokens.append(Token(TT_LSQUARE, posicion_inicial=self.pos))
        self.avanzar()
      elif self.caracter_actual == ']':
        tokens.append(Token(TT_RSQUARE, posicion_inicial=self.pos))
        self.avanzar()
      elif self.caracter_actual == '!':
        token, error = self.make_not_equals()
        if error: return [], error
        tokens.append(token)
      elif self.caracter_actual == '=':
        tokens.append(self.make_equals())
      elif self.caracter_actual == '<':
        tokens.append(self.make_less_than())
      elif self.caracter_actual == '>':
        tokens.append(self.make_greater_than())
      elif self.caracter_actual == ',':
        tokens.append(Token(TT_COMMA, posicion_inicial=self.pos))
        self.avanzar()
      else:
        posicion_inicial = self.pos.copy()
        char = self.caracter_actual
        self.avanzar()
        return [], ErrorCaracterIlegal(posicion_inicial, self.pos, "'" + char + "'")

    tokens.append(Token(TT_EOF, posicion_inicial=self.pos))
    return tokens, None

  def make_number(self):
    num_str = ''
    dot_count = 0
    posicion_inicial = self.pos.copy()

    while self.caracter_actual != None and self.caracter_actual in NUMEROS + '.':
      if self.caracter_actual == '.':
        if dot_count == 1: break
        dot_count += 1
      num_str += self.caracter_actual
      self.avanzar()

    if dot_count == 0:
      return Token(TT_INT, int(num_str), posicion_inicial, self.pos)
    else:
      return Token(TT_FLOAT, float(num_str), posicion_inicial, self.pos)

  def make_string(self):
    string = ''
    posicion_inicial = self.pos.copy()
    escape_character = False
    self.avanzar()

    escape_characters = {
      'n': '\n',
      't': '\t'
    }

    while self.caracter_actual != None and (self.caracter_actual != '"' or escape_character):
      if escape_character:
        string += escape_characters.get(self.caracter_actual, self.caracter_actual)
      else:
        if self.caracter_actual == '\\':
          escape_character = True
        else:
          string += self.caracter_actual
      self.avanzar()
      escape_character = False

    self.avanzar()
    return Token(TT_STRING, string, posicion_inicial, self.pos)

  def make_identifier(self):
    id_str = ''
    posicion_inicial = self.pos.copy()

    while self.caracter_actual != None and self.caracter_actual in LETRAS_NUMEROS + '_':
      id_str += self.caracter_actual
      self.avanzar()

    tok_type = TT_KEYWORD if id_str in KEYWORDS else TT_IDENTIFIER
    return Token(tok_type, id_str, posicion_inicial, self.pos)

  def make_minus_or_arrow(self):
    tok_type = TT_MINUS
    posicion_inicial = self.pos.copy()
    self.avanzar()

    if self.caracter_actual == '>':
      self.avanzar()
      tok_type = TT_ARROW

    return Token(tok_type, posicion_inicial=posicion_inicial, posicion_final=self.pos)

  def make_not_equals(self):
    posicion_inicial = self.pos.copy()
    self.avanzar()

    if self.caracter_actual == '=':
      self.avanzar()
      return Token(TT_NE, posicion_inicial=posicion_inicial, posicion_final=self.pos), None

    self.avanzar()
    return None, ErrorCaracterEsperado(posicion_inicial, self.pos, "'=' (after '!')")

  def make_equals(self):
    tok_type = TT_EQ
    posicion_inicial = self.pos.copy()
    self.avanzar()

    if self.caracter_actual == '=':
      self.avanzar()
      tok_type = TT_EE

    return Token(tok_type, posicion_inicial=posicion_inicial, posicion_final=self.pos)

  def make_less_than(self):
    tok_type = TT_LT
    posicion_inicial = self.pos.copy()
    self.avanzar()

    if self.caracter_actual == '=':
      self.avanzar()
      tok_type = TT_LTE

    return Token(tok_type, posicion_inicial=posicion_inicial, posicion_final=self.pos)

  def make_greater_than(self):
    tok_type = TT_GT
    posicion_inicial = self.pos.copy()
    self.avanzar()

    if self.caracter_actual == '=':
      self.avanzar()
      tok_type = TT_GTE

    return Token(tok_type, posicion_inicial=posicion_inicial, posicion_final=self.pos)

  def skip_comment(self):
    self.avanzar()

    while self.caracter_actual != '\n':
      self.avanzar()

    self.avanzar()

#######################################
# NODES
#######################################

class NumberNode:
  def __init__(self, tok):
    self.tok = tok

    self.posicion_inicial = self.tok.posicion_inicial
    self.posicion_final = self.tok.posicion_final

  def __repr__(self):
    return f'{self.tok}'

class StringNode:
  def __init__(self, tok):
    self.tok = tok

    self.posicion_inicial = self.tok.posicion_inicial
    self.posicion_final = self.tok.posicion_final

  def __repr__(self):
    return f'{self.tok}'

class ListNode:
  def __init__(self, element_nodes, posicion_inicial, posicion_final):
    self.element_nodes = element_nodes

    self.posicion_inicial = posicion_inicial
    self.posicion_final = posicion_final

class VarAccessNode:
  def __init__(self, var_name_tok):
    self.var_name_tok = var_name_tok

    self.posicion_inicial = self.var_name_tok.posicion_inicial
    self.posicion_final = self.var_name_tok.posicion_final

class VarAssignNode:
  def __init__(self, var_name_tok, value_node):
    self.var_name_tok = var_name_tok
    self.value_node = value_node

    self.posicion_inicial = self.var_name_tok.posicion_inicial
    self.posicion_final = self.value_node.posicion_final

class BinOpNode:
  def __init__(self, left_node, op_tok, right_node):
    self.left_node = left_node
    self.op_tok = op_tok
    self.right_node = right_node

    self.posicion_inicial = self.left_node.posicion_inicial
    self.posicion_final = self.right_node.posicion_final

  def __repr__(self):
    return f'({self.left_node}, {self.op_tok}, {self.right_node})'

class UnaryOpNode:
  def __init__(self, op_tok, node):
    self.op_tok = op_tok
    self.node = node

    self.posicion_inicial = self.op_tok.posicion_inicial
    self.posicion_final = node.posicion_final

  def __repr__(self):
    return f'({self.op_tok}, {self.node})'

class IfNode:
  def __init__(self, cases, else_case):
    self.cases = cases
    self.else_case = else_case

    self.posicion_inicial = self.cases[0][0].posicion_inicial
    self.posicion_final = (self.else_case or self.cases[len(self.cases) - 1])[0].posicion_final

class ForNode:
  def __init__(self, var_name_tok, start_value_node, end_value_node, step_value_node, body_node, should_return_null):
    self.var_name_tok = var_name_tok
    self.start_value_node = start_value_node
    self.end_value_node = end_value_node
    self.step_value_node = step_value_node
    self.body_node = body_node
    self.should_return_null = should_return_null

    self.posicion_inicial = self.var_name_tok.posicion_inicial
    self.posicion_final = self.body_node.posicion_final

class WhileNode:
  def __init__(self, condition_node, body_node, should_return_null):
    self.condition_node = condition_node
    self.body_node = body_node
    self.should_return_null = should_return_null

    self.posicion_inicial = self.condition_node.posicion_inicial
    self.posicion_final = self.body_node.posicion_final

class FuncDefNode:
  def __init__(self, var_name_tok, arg_name_toks, body_node, should_auto_return):
    self.var_name_tok = var_name_tok
    self.arg_name_toks = arg_name_toks
    self.body_node = body_node
    self.should_auto_return = should_auto_return

    if self.var_name_tok:
      self.posicion_inicial = self.var_name_tok.posicion_inicial
    elif len(self.arg_name_toks) > 0:
      self.posicion_inicial = self.arg_name_toks[0].posicion_inicial
    else:
      self.posicion_inicial = self.body_node.posicion_inicial

    self.posicion_final = self.body_node.posicion_final

class CallNode:
  def __init__(self, node_to_call, arg_nodes):
    self.node_to_call = node_to_call
    self.arg_nodes = arg_nodes

    self.posicion_inicial = self.node_to_call.posicion_inicial

    if len(self.arg_nodes) > 0:
      self.posicion_final = self.arg_nodes[len(self.arg_nodes) - 1].posicion_final
    else:
      self.posicion_final = self.node_to_call.posicion_final

class ReturnNode:
  def __init__(self, node_to_return, posicion_inicial, posicion_final):
    self.node_to_return = node_to_return

    self.posicion_inicial = posicion_inicial
    self.posicion_final = posicion_final

class ContinueNode:
  def __init__(self, posicion_inicial, posicion_final):
    self.posicion_inicial = posicion_inicial
    self.posicion_final = posicion_final

class BreakNode:
  def __init__(self, posicion_inicial, posicion_final):
    self.posicion_inicial = posicion_inicial
    self.posicion_final = posicion_final

#######################################
# PARSE resultado
#######################################

class ParseResultado:
  def __init__(self):
    self.error = None
    self.node = None
    self.last_registrared_avanzar_count = 0
    self.avanzar_count = 0
    self.to_reverse_count = 0

  def registrar_avance(self):
    self.last_registrared_avanzar_count = 1
    self.avanzar_count += 1

  def registrar(self, res):
    self.last_registrared_avanzar_count = res.avanzar_count
    self.avanzar_count += res.avanzar_count
    if res.error: self.error = res.error
    return res.node

  def try_registrar(self, res):
    if res.error:
      self.to_reverse_count = res.avanzar_count
      return None
    return self.registrar(res)

  def success(self, node):
    self.node = node
    return self

  def failure(self, error):
    if not self.error or self.last_registrared_avanzar_count == 0:
      self.error = error
    return self

#######################################
# ANALIZADOR SINTACTICO
#######################################

class Parser:
  def __init__(self, tokens):
    self.tokens = tokens
    self.tok_idx = -1
    self.avanzar()

  def avanzar(self):
    self.tok_idx += 1
    self.update_current_tok()
    return self.current_tok

  def reverse(self, amount=1):
    self.tok_idx -= amount
    self.update_current_tok()
    return self.current_tok

  def update_current_tok(self):
    if self.tok_idx >= 0 and self.tok_idx < len(self.tokens):
      self.current_tok = self.tokens[self.tok_idx]

  def parse(self):
    res = self.statements()
    if not res.error and self.current_tok.type != TT_EOF:
      return res.failure(ErrorSintaxisInvalida(
        self.current_tok.posicion_inicial, self.current_tok.posicion_final,
        "Token cannot appear after previous tokens"
      ))
    return res

  ###################################
  #para hacer varias lineas de codigo
  def statements(self):
    res = ParseResultado()
    statements = []
    posicion_inicial = self.current_tok.posicion_inicial.copy()

    while self.current_tok.type == TT_NEWLINE:
      res.registrar_avance()
      self.avanzar()

    statement = res.registrar(self.statement())
    if res.error: return res
    statements.append(statement)

    more_statements = True
    #busca mas lineas de codigo
    while True:
      newline_count = 0
      while self.current_tok.type == TT_NEWLINE:
        res.registrar_avance()
        self.avanzar()
        newline_count += 1
      if newline_count == 0:
        more_statements = False

      if not more_statements: break
      statement = res.try_registrar(self.statement())
      if not statement:
        self.reverse(res.to_reverse_count)
        more_statements = False
        continue
      statements.append(statement)

    return res.success(ListNode(
      statements,
      posicion_inicial,
      self.current_tok.posicion_final.copy()
    ))

  def statement(self):
    res = ParseResultado()
    posicion_inicial = self.current_tok.posicion_inicial.copy()

    if self.current_tok.matches(TT_KEYWORD, 'REVENO'):
      res.registrar_avance()
      self.avanzar()

      expr = res.try_registrar(self.expr())
      if not expr:
        self.reverse(res.to_reverse_count)
      return res.success(ReturnNode(expr, posicion_inicial, self.current_tok.posicion_inicial.copy()))

    if self.current_tok.matches(TT_KEYWORD, 'CONTINUE'):
      res.registrar_avance()
      self.avanzar()
      return res.success(ContinueNode(posicion_inicial, self.current_tok.posicion_inicial.copy()))

    if self.current_tok.matches(TT_KEYWORD, 'BREAK'):
      res.registrar_avance()
      self.avanzar()
      return res.success(BreakNode(posicion_inicial, self.current_tok.posicion_inicial.copy()))

    expr = res.registrar(self.expr())
    if res.error:
      return res.failure(ErrorSintaxisInvalida(
        self.current_tok.posicion_inicial, self.current_tok.posicion_final,
        "Expected 'REVENO', 'CONTINUE', 'BREAK', 'VAR', 'SE', 'POR', 'WHILE', 'FUNKCIO', int, float, identifier, '+', '-', '(', '[' or 'NOT'"
      ))
    return res.success(expr)

  def expr(self):
    res = ParseResultado()

    if self.current_tok.matches(TT_KEYWORD, 'VAR'):
      res.registrar_avance()
      self.avanzar()

      if self.current_tok.type != TT_IDENTIFIER:
        return res.failure(ErrorSintaxisInvalida(
          self.current_tok.posicion_inicial, self.current_tok.posicion_final,
          "Expected identifier"
        ))

      var_name = self.current_tok
      res.registrar_avance()
      self.avanzar()

      if self.current_tok.type != TT_EQ:
        return res.failure(ErrorSintaxisInvalida(
          self.current_tok.posicion_inicial, self.current_tok.posicion_final,
          "Expected '='"
        ))

      res.registrar_avance()
      self.avanzar()
      expr = res.registrar(self.expr())
      if res.error: return res
      return res.success(VarAssignNode(var_name, expr))

    node = res.registrar(self.bin_op(self.comp_expr, ((TT_KEYWORD, 'AND'), (TT_KEYWORD, 'OR'))))

    if res.error:
      return res.failure(ErrorSintaxisInvalida(
        self.current_tok.posicion_inicial, self.current_tok.posicion_final,
        "Expected 'VAR', 'SE', 'POR', 'WHILE', 'FUNKCIO', int, float, identifier, '+', '-', '(', '[' or 'NOT'"
      ))

    return res.success(node)

  def comp_expr(self):
    res = ParseResultado()

    if self.current_tok.matches(TT_KEYWORD, 'NOT'):
      op_tok = self.current_tok
      res.registrar_avance()
      self.avanzar()

      node = res.registrar(self.comp_expr())
      if res.error: return res
      return res.success(UnaryOpNode(op_tok, node))

    node = res.registrar(self.bin_op(self.arith_expr, (TT_EE, TT_NE, TT_LT, TT_GT, TT_LTE, TT_GTE)))

    if res.error:
      return res.failure(ErrorSintaxisInvalida(
        self.current_tok.posicion_inicial, self.current_tok.posicion_final,
        "Expected int, float, identifier, '+', '-', '(', '[', 'SE', 'POR', 'WHILE', 'FUNKCIO' or 'NOT'"
      ))

    return res.success(node)

  def arith_expr(self):
    return self.bin_op(self.term, (TT_PLUS, TT_MINUS))

  def term(self):
    return self.bin_op(self.factor, (TT_MUL, TT_DIV))

  def factor(self):
    res = ParseResultado()
    tok = self.current_tok

    if tok.type in (TT_PLUS, TT_MINUS):
      res.registrar_avance()
      self.avanzar()
      factor = res.registrar(self.factor())
      if res.error: return res
      return res.success(UnaryOpNode(tok, factor))

    return self.power()

  def power(self):
    return self.bin_op(self.call, (TT_POW, ), self.factor)

  def call(self):
    res = ParseResultado()
    atom = res.registrar(self.atom())
    if res.error: return res

    if self.current_tok.type == TT_LPAREN:
      res.registrar_avance()
      self.avanzar()
      arg_nodes = []

      if self.current_tok.type == TT_RPAREN:
        res.registrar_avance()
        self.avanzar()
      else:
        arg_nodes.append(res.registrar(self.expr()))
        if res.error:
          return res.failure(ErrorSintaxisInvalida(
            self.current_tok.posicion_inicial, self.current_tok.posicion_final,
            "Expected ')', 'VAR', 'SE', 'POR', 'WHILE', 'FUNKCIO', int, float, identifier, '+', '-', '(', '[' or 'NOT'"
          ))

        while self.current_tok.type == TT_COMMA:
          res.registrar_avance()
          self.avanzar()

          arg_nodes.append(res.registrar(self.expr()))
          if res.error: return res

        if self.current_tok.type != TT_RPAREN:
          return res.failure(ErrorSintaxisInvalida(
            self.current_tok.posicion_inicial, self.current_tok.posicion_final,
            f"Expected ',' or ')'"
          ))

        res.registrar_avance()
        self.avanzar()
      return res.success(CallNode(atom, arg_nodes))
    return res.success(atom)

  def atom(self):
    res = ParseResultado()
    tok = self.current_tok

    if tok.type in (TT_INT, TT_FLOAT):
      res.registrar_avance()
      self.avanzar()
      return res.success(NumberNode(tok))

    elif tok.type == TT_STRING:
      res.registrar_avance()
      self.avanzar()
      return res.success(StringNode(tok))

    elif tok.type == TT_IDENTIFIER:
      res.registrar_avance()
      self.avanzar()
      return res.success(VarAccessNode(tok))

    elif tok.type == TT_LPAREN:
      res.registrar_avance()
      self.avanzar()
      expr = res.registrar(self.expr())
      if res.error: return res
      if self.current_tok.type == TT_RPAREN:
        res.registrar_avance()
        self.avanzar()
        return res.success(expr)
      else:
        return res.failure(ErrorSintaxisInvalida(
          self.current_tok.posicion_inicial, self.current_tok.posicion_final,
          "Expected ')'"
        ))

    elif tok.type == TT_LSQUARE:
      list_expr = res.registrar(self.list_expr())
      if res.error: return res
      return res.success(list_expr)

    elif tok.matches(TT_KEYWORD, 'SE'):
      if_expr = res.registrar(self.if_expr())
      if res.error: return res
      return res.success(if_expr)

    elif tok.matches(TT_KEYWORD, 'POR'):
      for_expr = res.registrar(self.for_expr())
      if res.error: return res
      return res.success(for_expr)

    elif tok.matches(TT_KEYWORD, 'WHILE'):
      while_expr = res.registrar(self.while_expr())
      if res.error: return res
      return res.success(while_expr)

    elif tok.matches(TT_KEYWORD, 'FUNKCIO'):
      func_def = res.registrar(self.func_def())
      if res.error: return res
      return res.success(func_def)

    return res.failure(ErrorSintaxisInvalida(
      tok.posicion_inicial, tok.posicion_final,
      "Expected int, float, identifier, '+', '-', '(', '[', IF', 'POR', 'WHILE', 'FUNKCIO'"
    ))

  def list_expr(self):
    res = ParseResultado()
    element_nodes = []
    posicion_inicial = self.current_tok.posicion_inicial.copy()

    if self.current_tok.type != TT_LSQUARE:
      return res.failure(ErrorSintaxisInvalida(
        self.current_tok.posicion_inicial, self.current_tok.posicion_final,
        f"Expected '['"
      ))

    res.registrar_avance()
    self.avanzar()

    if self.current_tok.type == TT_RSQUARE:
      res.registrar_avance()
      self.avanzar()
    else:
      element_nodes.append(res.registrar(self.expr()))
      if res.error:
        return res.failure(ErrorSintaxisInvalida(
          self.current_tok.posicion_inicial, self.current_tok.posicion_final,
          "Expected ']', 'VAR', 'SE', 'POR', 'WHILE', 'FUNKCIO', int, float, identifier, '+', '-', '(', '[' or 'NOT'"
        ))

      while self.current_tok.type == TT_COMMA:
        res.registrar_avance()
        self.avanzar()

        element_nodes.append(res.registrar(self.expr()))
        if res.error: return res

      if self.current_tok.type != TT_RSQUARE:
        return res.failure(ErrorSintaxisInvalida(
          self.current_tok.posicion_inicial, self.current_tok.posicion_final,
          f"Expected ',' or ']'"
        ))

      res.registrar_avance()
      self.avanzar()

    return res.success(ListNode(
      element_nodes,
      posicion_inicial,
      self.current_tok.posicion_final.copy()
    ))

  def if_expr(self):
    res = ParseResultado()
    all_cases = res.registrar(self.if_expr_cases('SE'))
    if res.error: return res
    cases, else_case = all_cases
    return res.success(IfNode(cases, else_case))

  def if_expr_b(self):
    return self.if_expr_cases('ELIF')

  def if_expr_c(self):
    res = ParseResultado()
    else_case = None

    if self.current_tok.matches(TT_KEYWORD, 'ELSE'):
      res.registrar_avance()
      self.avanzar()

      if self.current_tok.type == TT_NEWLINE:
        res.registrar_avance()
        self.avanzar()

        statements = res.registrar(self.statements())
        if res.error: return res
        else_case = (statements, True)

        if self.current_tok.matches(TT_KEYWORD, 'FINI'):
          res.registrar_avance()
          self.avanzar()
        else:
          return res.failure(ErrorSintaxisInvalida(
            self.current_tok.posicion_inicial, self.current_tok.posicion_final,
            "Expected 'FINI'"
          ))
      else:
        expr = res.registrar(self.statement())
        if res.error: return res
        else_case = (expr, False)

    return res.success(else_case)

  def if_expr_b_or_c(self):
    res = ParseResultado()
    cases, else_case = [], None

    if self.current_tok.matches(TT_KEYWORD, 'ELIF'):
      all_cases = res.registrar(self.if_expr_b())
      if res.error: return res
      cases, else_case = all_cases
    else:
      else_case = res.registrar(self.if_expr_c())
      if res.error: return res

    return res.success((cases, else_case))

  def if_expr_cases(self, case_keyword):
    res = ParseResultado()
    cases = []
    else_case = None

    if not self.current_tok.matches(TT_KEYWORD, case_keyword):
      return res.failure(ErrorSintaxisInvalida(
        self.current_tok.posicion_inicial, self.current_tok.posicion_final,
        f"Expected '{case_keyword}'"
      ))

    res.registrar_avance()
    self.avanzar()

    condition = res.registrar(self.expr())
    if res.error: return res

    if not self.current_tok.matches(TT_KEYWORD, 'DO'):
      return res.failure(ErrorSintaxisInvalida(
        self.current_tok.posicion_inicial, self.current_tok.posicion_final,
        f"Expected 'DO'"
      ))

    res.registrar_avance()
    self.avanzar()

    if self.current_tok.type == TT_NEWLINE:
      res.registrar_avance()
      self.avanzar()

      statements = res.registrar(self.statements())
      if res.error: return res
      cases.appFINI((condition, statements, True))

      if self.current_tok.matches(TT_KEYWORD, 'FINI'):
        res.registrar_avance()
        self.avanzar()
      else:
        all_cases = res.registrar(self.if_expr_b_or_c())
        if res.error: return res
        new_cases, else_case = all_cases
        cases.extend(new_cases)
    else:
      expr = res.registrar(self.statement())
      if res.error: return res
      cases.append((condition, expr, False))

      all_cases = res.registrar(self.if_expr_b_or_c())
      if res.error: return res
      new_cases, else_case = all_cases
      cases.extend(new_cases)

    return res.success((cases, else_case))

  def for_expr(self):
    res = ParseResultado()

    if not self.current_tok.matches(TT_KEYWORD, 'POR'):
      return res.failure(ErrorSintaxisInvalida(
        self.current_tok.posicion_inicial, self.current_tok.posicion_final,
        f"Expected 'POR'"
      ))

    res.registrar_avance()
    self.avanzar()

    if self.current_tok.type != TT_IDENTIFIER:
      return res.failure(ErrorSintaxisInvalida(
        self.current_tok.posicion_inicial, self.current_tok.posicion_final,
        f"Expected identifier"
      ))

    var_name = self.current_tok
    res.registrar_avance()
    self.avanzar()

    if self.current_tok.type != TT_EQ:
      return res.failure(ErrorSintaxisInvalida(
        self.current_tok.posicion_inicial, self.current_tok.posicion_final,
        f"Expected '='"
      ))

    res.registrar_avance()
    self.avanzar()

    start_value = res.registrar(self.expr())
    if res.error: return res

    if not self.current_tok.matches(TT_KEYWORD, 'AL'):
      return res.failure(ErrorSintaxisInvalida(
        self.current_tok.posicion_inicial, self.current_tok.posicion_final,
        f"Expected 'AL'"
      ))

    res.registrar_avance()
    self.avanzar()

    end_value = res.registrar(self.expr())
    if res.error: return res

    if self.current_tok.matches(TT_KEYWORD, 'STEP'):
      res.registrar_avance()
      self.avanzar()

      step_value = res.registrar(self.expr())
      if res.error: return res
    else:
      step_value = None

    if not self.current_tok.matches(TT_KEYWORD, 'DO'):
      return res.failure(ErrorSintaxisInvalida(
        self.current_tok.posicion_inicial, self.current_tok.posicion_final,
        f"Expected 'DO'"
      ))

    res.registrar_avance()
    self.avanzar()

    if self.current_tok.type == TT_NEWLINE:
      res.registrar_avance()
      self.avanzar()

      body = res.registrar(self.statements())
      if res.error: return res

      if not self.current_tok.matches(TT_KEYWORD, 'FINI'):
        return res.failure(ErrorSintaxisInvalida(
          self.current_tok.posicion_inicial, self.current_tok.posicion_final,
          f"Expected 'FINI'"
        ))

      res.registrar_avance()
      self.avanzar()

      return res.success(ForNode(var_name, start_value, end_value, step_value, body, True))

    body = res.registrar(self.statement())
    if res.error: return res

    return res.success(ForNode(var_name, start_value, end_value, step_value, body, False))

  def while_expr(self):
    res = ParseResultado()

    if not self.current_tok.matches(TT_KEYWORD, 'WHILE'):
      return res.failure(ErrorSintaxisInvalida(
        self.current_tok.posicion_inicial, self.current_tok.posicion_final,
        f"Expected 'WHILE'"
      ))

    res.registrar_avance()
    self.avanzar()

    condition = res.registrar(self.expr())
    if res.error: return res

    if not self.current_tok.matches(TT_KEYWORD, 'DO'):
      return res.failure(ErrorSintaxisInvalida(
        self.current_tok.posicion_inicial, self.current_tok.posicion_final,
        f"Expected 'DO'"
      ))

    res.registrar_avance()
    self.avanzar()

    if self.current_tok.type == TT_NEWLINE:
      res.registrar_avance()
      self.avanzar()

      body = res.registrar(self.statements())
      if res.error: return res

      if not self.current_tok.matches(TT_KEYWORD, 'FINI'):
        return res.failure(ErrorSintaxisInvalida(
          self.current_tok.posicion_inicial, self.current_tok.posicion_final,
          f"Expected 'FINI'"
        ))

      res.registrar_avance()
      self.avanzar()

      return res.success(WhileNode(condition, body, True))

    body = res.registrar(self.statement())
    if res.error: return res

    return res.success(WhileNode(condition, body, False))

  def func_def(self):
    res = ParseResultado()

    if not self.current_tok.matches(TT_KEYWORD, 'FUNKCIO'):
      return res.failure(ErrorSintaxisInvalida(
        self.current_tok.posicion_inicial, self.current_tok.posicion_final,
        f"Expected 'FUNKCIO'"
      ))

    res.registrar_avance()
    self.avanzar()

    if self.current_tok.type == TT_IDENTIFIER:
      var_name_tok = self.current_tok
      res.registrar_avance()
      self.avanzar()
      if self.current_tok.type != TT_LPAREN:
        return res.failure(ErrorSintaxisInvalida(
          self.current_tok.posicion_inicial, self.current_tok.posicion_final,
          f"Expected '('"
        ))
    else:
      var_name_tok = None
      if self.current_tok.type != TT_LPAREN:
        return res.failure(ErrorSintaxisInvalida(
          self.current_tok.posicion_inicial, self.current_tok.posicion_final,
          f"Expected identifier or '('"
        ))

    res.registrar_avance()
    self.avanzar()
    arg_name_toks = []

    if self.current_tok.type == TT_IDENTIFIER:
      arg_name_toks.append(self.current_tok)
      res.registrar_avance()
      self.avanzar()

      while self.current_tok.type == TT_COMMA:
        res.registrar_avance()
        self.avanzar()

        if self.current_tok.type != TT_IDENTIFIER:
          return res.failure(ErrorSintaxisInvalida(
            self.current_tok.posicion_inicial, self.current_tok.posicion_final,
            f"Expected identifier"
          ))

        arg_name_toks.append(self.current_tok)
        res.registrar_avance()
        self.avanzar()

      if self.current_tok.type != TT_RPAREN:
        return res.failure(ErrorSintaxisInvalida(
          self.current_tok.posicion_inicial, self.current_tok.posicion_final,
          f"Expected ',' or ')'"
        ))
    else:
      if self.current_tok.type != TT_RPAREN:
        return res.failure(ErrorSintaxisInvalida(
          self.current_tok.posicion_inicial, self.current_tok.posicion_final,
          f"Expected identifier or ')'"
        ))

    res.registrar_avance()
    self.avanzar()

    if self.current_tok.type == TT_ARROW:
      res.registrar_avance()
      self.avanzar()

      body = res.registrar(self.expr())
      if res.error: return res

      return res.success(FuncDefNode(
        var_name_tok,
        arg_name_toks,
        body,
        True
      ))

    if self.current_tok.type != TT_NEWLINE:
      return res.failure(ErrorSintaxisInvalida(
        self.current_tok.posicion_inicial, self.current_tok.posicion_final,
        f"Expected '->' or NEWLINE"
      ))

    res.registrar_avance()
    self.avanzar()

    body = res.registrar(self.statements())
    if res.error: return res

    if not self.current_tok.matches(TT_KEYWORD, 'FINI'):
      return res.failure(ErrorSintaxisInvalida(
        self.current_tok.posicion_inicial, self.current_tok.posicion_final,
        f"Expected 'FINI'"
      ))

    res.registrar_avance()
    self.avanzar()

    return res.success(FuncDefNode(
      var_name_tok,
      arg_name_toks,
      body,
      False
    ))

  ###################################

  def bin_op(self, func_a, ops, func_b=None):
    if func_b == None:
      func_b = func_a

    res = ParseResultado()
    left = res.registrar(func_a())
    if res.error: return res

    while self.current_tok.type in ops or (self.current_tok.type, self.current_tok.value) in ops:
      op_tok = self.current_tok
      res.registrar_avance()
      self.avanzar()
      right = res.registrar(func_b())
      if res.error: return res
      left = BinOpNode(left, op_tok, right)

    return res.success(left)

#######################################
# RUNTIME resultado
#######################################

class RTresultado:
  def __init__(self):
    self.reset()

  def reset(self):
    self.value = None
    self.error = None
    self.func_return_value = None
    self.loop_should_continue = False
    self.loop_should_break = False

  def registrar(self, res):
    self.error = res.error
    self.func_return_value = res.func_return_value
    self.loop_should_continue = res.loop_should_continue
    self.loop_should_break = res.loop_should_break
    return res.value

  def success(self, value):
    self.reset()
    self.value = value
    return self

  def success_return(self, value):
    self.reset()
    self.func_return_value = value
    return self

  def success_continue(self):
    self.reset()
    self.loop_should_continue = True
    return self

  def success_break(self):
    self.reset()
    self.loop_should_break = True
    return self

  def failure(self, error):
    self.reset()
    self.error = error
    return self

  def should_return(self):
    # Note: this will allow you to continue and break outside the current function
    return (
      self.error or
      self.func_return_value or
      self.loop_should_continue or
      self.loop_should_break
    )

#######################################
# VALUES
#######################################

class Value:
  def __init__(self):
    self.set_pos()
    self.set_contexto()

  def set_pos(self, posicion_inicial=None, posicion_final=None):
    self.posicion_inicial = posicion_inicial
    self.posicion_final = posicion_final
    return self

  def set_contexto(self, contexto=None):
    self.contexto = contexto
    return self

  def sumando_con(self, other):
    return None, self.illegal_operation(other)

  def restando_con(self, other):
    return None, self.illegal_operation(other)

  def multiplicando_por(self, other):
    return None, self.illegal_operation(other)

  def dividiendo_por(self, other):
    return None, self.illegal_operation(other)

  def powed_by(self, other):
    return None, self.illegal_operation(other)

  def get_comparison_eq(self, other):
    return None, self.illegal_operation(other)

  def get_comparison_ne(self, other):
    return None, self.illegal_operation(other)

  def get_comparison_lt(self, other):
    return None, self.illegal_operation(other)

  def get_comparison_gt(self, other):
    return None, self.illegal_operation(other)

  def get_comparison_lte(self, other):
    return None, self.illegal_operation(other)

  def get_comparison_gte(self, other):
    return None, self.illegal_operation(other)

  def anded_by(self, other):
    return None, self.illegal_operation(other)

  def ored_by(self, other):
    return None, self.illegal_operation(other)

  def notted(self, other):
    return None, self.illegal_operation(other)

  def execute(self, args):
    return RTresultado().failure(self.illegal_operation())

  def copy(self):
    raise Exception('No copy method defined')

  def is_true(self):
    return False

  def illegal_operation(self, other=None):
    if not other: other = self
    return ErrorTiempoEjecucion(
      self.posicion_inicial, other.posicion_final,
      'Illegal operation',
      self.contexto
    )

class Number(Value):
  def __init__(self, value):
    super().__init__()
    self.value = value

  def sumando_con(self, other):
    if isinstance(other, Number):
      return Number(self.value + other.value).set_contexto(self.contexto), None
    else:
      return None, Value.illegal_operation(self, other)

  def restando_con(self, other):
    if isinstance(other, Number):
      return Number(self.value - other.value).set_contexto(self.contexto), None
    else:
      return None, Value.illegal_operation(self, other)

  def multiplicando_por(self, other):
    if isinstance(other, Number):
      return Number(self.value * other.value).set_contexto(self.contexto), None
    else:
      return None, Value.illegal_operation(self, other)

  def dividiendo_por(self, other):
    if isinstance(other, Number):
      if other.value == 0:
        return None, ErrorTiempoEjecucion(
          other.posicion_inicial, other.posicion_final,
          'Division por cero',
          self.contexto
        )

      return Number(self.value / other.value).set_contexto(self.contexto), None
    else:
      return None, Value.illegal_operation(self, other)

  def powed_by(self, other):
    if isinstance(other, Number):
      return Number(self.value ** other.value).set_contexto(self.contexto), None
    else:
      return None, Value.illegal_operation(self, other)

  def get_comparison_eq(self, other):
    if isinstance(other, Number):
      return Number(int(self.value == other.value)).set_contexto(self.contexto), None
    else:
      return None, Value.illegal_operation(self, other)

  def get_comparison_ne(self, other):
    if isinstance(other, Number):
      return Number(int(self.value != other.value)).set_contexto(self.contexto), None
    else:
      return None, Value.illegal_operation(self, other)

  def get_comparison_lt(self, other):
    if isinstance(other, Number):
      return Number(int(self.value < other.value)).set_contexto(self.contexto), None
    else:
      return None, Value.illegal_operation(self, other)

  def get_comparison_gt(self, other):
    if isinstance(other, Number):
      return Number(int(self.value > other.value)).set_contexto(self.contexto), None
    else:
      return None, Value.illegal_operation(self, other)

  def get_comparison_lte(self, other):
    if isinstance(other, Number):
      return Number(int(self.value <= other.value)).set_contexto(self.contexto), None
    else:
      return None, Value.illegal_operation(self, other)

  def get_comparison_gte(self, other):
    if isinstance(other, Number):
      return Number(int(self.value >= other.value)).set_contexto(self.contexto), None
    else:
      return None, Value.illegal_operation(self, other)

  def anded_by(self, other):
    if isinstance(other, Number):
      return Number(int(self.value and other.value)).set_contexto(self.contexto), None
    else:
      return None, Value.illegal_operation(self, other)

  def ored_by(self, other):
    if isinstance(other, Number):
      return Number(int(self.value or other.value)).set_contexto(self.contexto), None
    else:
      return None, Value.illegal_operation(self, other)

  def notted(self):
    return Number(1 if self.value == 0 else 0).set_contexto(self.contexto), None

  def copy(self):
    copy = Number(self.value)
    copy.set_pos(self.posicion_inicial, self.posicion_final)
    copy.set_contexto(self.contexto)
    return copy

  def is_true(self):
    return self.value != 0

  def __str__(self):
    return str(self.value)

  def __repr__(self):
    return str(self.value)

Number.null = Number(0)
Number.false = Number(0)
Number.true = Number(1)
Number.math_PI = Number(math.pi)

class String(Value):
  def __init__(self, value):
    super().__init__()
    self.value = value

  def sumando_con(self, other):
    if isinstance(other, String):
      return String(self.value + other.value).set_contexto(self.contexto), None
    else:
      return None, Value.illegal_operation(self, other)

  def multiplicando_por(self, other):
    if isinstance(other, Number):
      return String(self.value * other.value).set_contexto(self.contexto), None
    else:
      return None, Value.illegal_operation(self, other)

  def is_true(self):
    return len(self.value) > 0

  def copy(self):
    copy = String(self.value)
    copy.set_pos(self.posicion_inicial, self.posicion_final)
    copy.set_contexto(self.contexto)
    return copy

  def __str__(self):
    return self.value

  def __repr__(self):
    return f'"{self.value}"'

class List(Value):
  def __init__(self, elements):
    super().__init__()
    self.elements = elements

#para agregar un valor
  def sumando_con(self, other):
    new_list = self.copy()
    new_list.elements.append(other)
    return new_list, None

#para quitar un elemento de la List
  def restando_con(self, other):
    if isinstance(other, Number):
      new_list = self.copy()
      try:
        new_list.elements.pop(other.value)
        return new_list, None
      except:
        return None, ErrorTiempoEjecucion(
          other.posicion_inicial, other.posicion_final,
          'El elemento con este índice no se pudo eliminar de la lista porque el índice está fuera de los límites',
          self.contexto
        )
    else:
      return None, Value.illegal_operation(self, other)
  
  #para agregar otra List
  def multiplicando_por(self, other):
    if isinstance(other, List):
      new_list = self.copy()
      new_list.elements.extend(other.elements)
      return new_list, None
    else:
      return None, Value.illegal_operation(self, other)
  
  #para obtener un elemento de la List
  def dividiendo_por(self, other):
    if isinstance(other, Number):
      try:
        return self.elements[other.value], None
      except:
        return None, ErrorTiempoEjecucion(
          other.posicion_inicial, other.posicion_final,
          'El elemento con este índice no se pudo obtener de la lista porque el índice está fuera de los límites',
          self.contexto
        )
    else:
      return None, Value.illegal_operation(self, other)

  def copy(self):
    copy = List(self.elements)
    copy.set_pos(self.posicion_inicial, self.posicion_final)
    copy.set_contexto(self.contexto)
    return copy

  def __str__(self):
    return ", ".join([str(x) for x in self.elements])

  def __repr__(self):
    return f'[{", ".join([repr(x) for x in self.elements])}]'

class BaseFunction(Value):
  def __init__(self, name):
    super().__init__()
    self.name = name or "<anonymous>"

  def generate_new_contexto(self):
    new_contexto = Context(self.name, self.contexto, self.posicion_inicial)
    new_contexto.symbol_table = SymbolTable(new_contexto.parent.symbol_table)
    return new_contexto

  def check_args(self, arg_names, args):
    res = RTresultado()

    if len(args) > len(arg_names):
      return res.failure(ErrorTiempoEjecucion(
        self.posicion_inicial, self.posicion_final,
        f"{len(args) - len(arg_names)} too many args passed into {self}",
        self.contexto
      ))

    if len(args) < len(arg_names):
      return res.failure(ErrorTiempoEjecucion(
        self.posicion_inicial, self.posicion_final,
        f"{len(arg_names) - len(args)} too few args passed into {self}",
        self.contexto
      ))

    return res.success(None)

  def populate_args(self, arg_names, args, exec_ctx):
    for i in range(len(args)):
      arg_name = arg_names[i]
      arg_value = args[i]
      arg_value.set_contexto(exec_ctx)
      exec_ctx.symbol_table.set(arg_name, arg_value)

  def check_and_populate_args(self, arg_names, args, exec_ctx):
    res = RTresultado()
    res.registrar(self.check_args(arg_names, args))
    if res.should_return(): return res
    self.populate_args(arg_names, args, exec_ctx)
    return res.success(None)

class Function(BaseFunction):
  def __init__(self, name, body_node, arg_names, should_auto_return):
    super().__init__(name)
    self.body_node = body_node
    self.arg_names = arg_names
    self.should_auto_return = should_auto_return

  def execute(self, args):
    res = RTresultado()
    semantico = Semantico()
    exec_ctx = self.generate_new_contexto()

    res.registrar(self.check_and_populate_args(self.arg_names, args, exec_ctx))
    if res.should_return(): return res

    value = res.registrar(semantico.recorre(self.body_node, exec_ctx))
    if res.should_return() and res.func_return_value == None: return res

    ret_value = (value if self.should_auto_return else None) or res.func_return_value or Number.null
    return res.success(ret_value)

  def copy(self):
    copy = Function(self.name, self.body_node, self.arg_names, self.should_auto_return)
    copy.set_contexto(self.contexto)
    copy.set_pos(self.posicion_inicial, self.posicion_final)
    return copy

  def __repr__(self):
    return f"<function {self.name}>"

class FuncionBuiltIn(BaseFunction):
  def __init__(self, name):
    super().__init__(name)

  def execute(self, args):
    res = RTresultado()
    exec_ctx = self.generate_new_contexto()

    method_name = f'execute_{self.name}'
    method = getattr(self, method_name, self.no_recorre_method)

    res.registrar(self.check_and_populate_args(method.arg_names, args, exec_ctx))
    if res.should_return(): return res

    return_value = res.registrar(method(exec_ctx))
    if res.should_return(): return res
    return res.success(return_value)

  def no_recorre_method(self, node, contexto):
    raise Exception(f'No execute_{self.name} method defined')

  def copy(self):
    copy = FuncionBuiltIn(self.name)
    copy.set_contexto(self.contexto)
    copy.set_pos(self.posicion_inicial, self.posicion_final)
    return copy

  def __repr__(self):
    return f"<built-in function {self.name}>"

  #####################################

  def execute_print(self, exec_ctx):
    global output
    output =str(exec_ctx.symbol_table.get('value'))
    print(output)
    return RTresultado().success(Number.null)
  execute_print.arg_names = ['value']


  def execute_is_number(self, exec_ctx):
    is_number = isinstance(exec_ctx.symbol_table.get("value"), Number)
    return RTresultado().success(Number.true if is_number else Number.false)
  execute_is_number.arg_names = ["value"]

  def execute_is_string(self, exec_ctx):
    is_number = isinstance(exec_ctx.symbol_table.get("value"), String)
    return RTresultado().success(Number.true if is_number else Number.false)
  execute_is_string.arg_names = ["value"]





  def execute_extend(self, exec_ctx):
    listA = exec_ctx.symbol_table.get("listA")
    listB = exec_ctx.symbol_table.get("listB")

    if not isinstance(listA, List):
      return RTresultado().failure(ErrorTiempoEjecucion(
        self.posicion_inicial, self.posicion_final,
        "First argument must be list",
        exec_ctx
      ))

    if not isinstance(listB, List):
      return RTresultado().failure(ErrorTiempoEjecucion(
        self.posicion_inicial, self.posicion_final,
        "Second argument must be list",
        exec_ctx
      ))

    listA.elements.extend(listB.elements)
    return RTresultado().success(Number.null)
  execute_extend.arg_names = ["listA", "listB"]

  def execute_len(self, exec_ctx):
    list_ = exec_ctx.symbol_table.get("list")

    if not isinstance(list_, List):
      return RTresultado().failure(ErrorTiempoEjecucion(
        self.posicion_inicial, self.posicion_final,
        "Argument must be list",
        exec_ctx
      ))

    return RTresultado().success(Number(len(list_.elements)))
  execute_len.arg_names = ["list"]

  def execute_run(self, exec_ctx):
    nombre_archivo = exec_ctx.symbol_table.get("nombre_archivo")

    if not isinstance(nombre_archivo, String):
      return RTresultado().failure(ErrorTiempoEjecucion(
        self.posicion_inicial, self.posicion_final,
        "Second argument must be string",
        exec_ctx
      ))

    nombre_archivo = nombre_archivo.value

    try:
      with open(nombre_archivo, "r") as f:
        script = f.read()
    except Exception as e:
      return RTresultado().failure(ErrorTiempoEjecucion(
        self.posicion_inicial, self.posicion_final,
        f"Failed to load script \"{nombre_archivo}\"\n" + str(e),
        exec_ctx
      ))

    _, error = run(nombre_archivo, script)

    if error:
      return RTresultado().failure(ErrorTiempoEjecucion(
        self.posicion_inicial, self.posicion_final,
        f"Failed to finish executing script \"{nombre_archivo}\"\n" +
        error.as_string(),
        exec_ctx
      ))

    return RTresultado().success(Number.null)
  execute_run.arg_names = ["nombre_archivo"]

FuncionBuiltIn.print       = FuncionBuiltIn("print")
FuncionBuiltIn.print_ret   = FuncionBuiltIn("print_ret")
FuncionBuiltIn.input       = FuncionBuiltIn("input")
FuncionBuiltIn.input_int   = FuncionBuiltIn("input_int")
FuncionBuiltIn.is_number   = FuncionBuiltIn("is_number")
FuncionBuiltIn.is_string   = FuncionBuiltIn("is_string")
FuncionBuiltIn.is_list     = FuncionBuiltIn("is_list")
FuncionBuiltIn.is_function = FuncionBuiltIn("is_function")
FuncionBuiltIn.append      = FuncionBuiltIn("append")
FuncionBuiltIn.extend      = FuncionBuiltIn("extend")
FuncionBuiltIn.len		   = FuncionBuiltIn("len")
FuncionBuiltIn.run		   = FuncionBuiltIn("run")

#######################################
# CONTEXT
#######################################

class Context:
  def __init__(self, display_name, parent=None, parent_entry_pos=None):
    self.display_name = display_name
    self.parent = parent
    self.parent_entry_pos = parent_entry_pos
    self.symbol_table = None

#######################################
# SYMBOL TABLE
#######################################

class SymbolTable:
  def __init__(self, parent=None):
    self.symbols = {}
    self.parent = parent

  def get(self, name):
    value = self.symbols.get(name, None)
    if value == None and self.parent:
      return self.parent.get(name)
    return value

  def set(self, name, value):
    self.symbols[name] = value

  def remove(self, name):
    del self.symbols[name]

#######################################
# ANALIZADOR SEMANTICO
#######################################

class Semantico:
  #para recorrer los nodos
  def recorre(self, node, contexto):
    method_name = f'recorre_{type(node).__name__}'
    method = getattr(self, method_name, self.no_recorre_method)
    return method(node, contexto)

  def no_recorre_method(self, node, contexto):
    raise Exception(f'No recorre_{type(node).__name__} method defined')

  ###################################
  #methods de recorrea para cada tipo de nodo numero
  def recorre_NumberNode(self, node, contexto):
    return RTresultado().success(
      Number(node.tok.value).set_contexto(contexto).set_pos(node.posicion_inicial, node.posicion_final)
    )

  def recorre_StringNode(self, node, contexto):
    return RTresultado().success(
      String(node.tok.value).set_contexto(contexto).set_pos(node.posicion_inicial, node.posicion_final)
    )
  #para cuando recorreamos un nodo List
  def recorre_ListNode(self, node, contexto):
    res = RTresultado()
    elements = []

    for element_node in node.element_nodes:
      elements.append(res.registrar(self.recorre(element_node, contexto)))
      if res.should_return(): return res

    return res.success(
      List(elements).set_contexto(contexto).set_pos(node.posicion_inicial, node.posicion_final)
    )

  def recorre_VarAccessNode(self, node, contexto):
    res = RTresultado()
    var_name = node.var_name_tok.value
    value = contexto.symbol_table.get(var_name)

    if not value:
      return res.failure(ErrorTiempoEjecucion(
        node.posicion_inicial, node.posicion_final,
        f"'{var_name}' is not defined",
        contexto
      ))

    value = value.copy().set_pos(node.posicion_inicial, node.posicion_final).set_contexto(contexto)
    return res.success(value)

  def recorre_VarAssignNode(self, node, contexto):
    res = RTresultado()
    var_name = node.var_name_tok.value
    value = res.registrar(self.recorre(node.value_node, contexto))
    if res.should_return(): return res

    contexto.symbol_table.set(var_name, value)
    return res.success(value)

  def recorre_BinOpNode(self, node, contexto):
    res = RTresultado()
    #como es binario, recorre a la derecha y a la izquierda
    left = res.registrar(self.recorre(node.left_node, contexto))
    if res.should_return(): return res
    right = res.registrar(self.recorre(node.right_node, contexto))
    if res.should_return(): return res
    
    #Trabaja con las diferentes tokens
    if node.op_tok.type == TT_PLUS:
      resultado, error = left.sumando_con(right)
    elif node.op_tok.type == TT_MINUS:
      resultado, error = left.restando_con(right)
    elif node.op_tok.type == TT_MUL:
      resultado, error = left.multiplicando_por(right)
    elif node.op_tok.type == TT_DIV:
      resultado, error = left.dividiendo_por(right)
    elif node.op_tok.type == TT_POW:
      resultado, error = left.powed_by(right)
    elif node.op_tok.type == TT_EE:
      resultado, error = left.get_comparison_eq(right)
    elif node.op_tok.type == TT_NE:
      resultado, error = left.get_comparison_ne(right)
    elif node.op_tok.type == TT_LT:
      resultado, error = left.get_comparison_lt(right)
    elif node.op_tok.type == TT_GT:
      resultado, error = left.get_comparison_gt(right)
    elif node.op_tok.type == TT_LTE:
      resultado, error = left.get_comparison_lte(right)
    elif node.op_tok.type == TT_GTE:
      resultado, error = left.get_comparison_gte(right)
    elif node.op_tok.matches(TT_KEYWORD, 'AND'):
      resultado, error = left.anded_by(right)
    elif node.op_tok.matches(TT_KEYWORD, 'OR'):
      resultado, error = left.ored_by(right)

    if error:
      return res.failure(error)
    else:
      return res.success(resultado.set_pos(node.posicion_inicial, node.posicion_final))

  def recorre_UnaryOpNode(self, node, contexto):
    res = RTresultado()
    #como es unitario, vuelve a si mismo
    number = res.registrar(self.recorre(node.node, contexto))
    if res.should_return(): return res

    error = None

    if node.op_tok.type == TT_MINUS:
      number, error = number.multiplicando_por(Number(-1))
    elif node.op_tok.matches(TT_KEYWORD, 'NOT'):
      number, error = number.notted()

    if error:
      return res.failure(error)
    else:
      return res.success(number.set_pos(node.posicion_inicial, node.posicion_final))

  def recorre_IfNode(self, node, contexto):
    res = RTresultado()

    for condition, expr, should_return_null in node.cases:
      condition_value = res.registrar(self.recorre(condition, contexto))
      if res.should_return(): return res

      if condition_value.is_true():
        expr_value = res.registrar(self.recorre(expr, contexto))
        if res.should_return(): return res
        return res.success(Number.null if should_return_null else expr_value)

    if node.else_case:
      expr, should_return_null = node.else_case
      expr_value = res.registrar(self.recorre(expr, contexto))
      if res.should_return(): return res
      return res.success(Number.null if should_return_null else expr_value)

    return res.success(Number.null)

  def recorre_ForNode(self, node, contexto):
    res = RTresultado()
    elements = []

    start_value = res.registrar(self.recorre(node.start_value_node, contexto))
    if res.should_return(): return res

    end_value = res.registrar(self.recorre(node.end_value_node, contexto))
    if res.should_return(): return res

    if node.step_value_node:
      step_value = res.registrar(self.recorre(node.step_value_node, contexto))
      if res.should_return(): return res
    else:
      step_value = Number(1)

    i = start_value.value

    if step_value.value >= 0:
      condition = lambda: i < end_value.value
    else:
      condition = lambda: i > end_value.value

    while condition():
      contexto.symbol_table.set(node.var_name_tok.value, Number(i))
      i += step_value.value

      value = res.registrar(self.recorre(node.body_node, contexto))
      if res.should_return() and res.loop_should_continue == False and res.loop_should_break == False: return res

      if res.loop_should_continue:
        continue

      if res.loop_should_break:
        break

      elements.append(value)

    return res.success(
      Number.null if node.should_return_null else
      List(elements).set_contexto(contexto).set_pos(node.posicion_inicial, node.posicion_final)
    )

  def recorre_WhileNode(self, node, contexto):
    res = RTresultado()
    elements = []

    while True:
      condition = res.registrar(self.recorre(node.condition_node, contexto))
      if res.should_return(): return res

      if not condition.is_true():
        break

      value = res.registrar(self.recorre(node.body_node, contexto))
      if res.should_return() and res.loop_should_continue == False and res.loop_should_break == False: return res

      if res.loop_should_continue:
        continue

      if res.loop_should_break:
        break

      elements.append(value)

    return res.success(
      Number.null if node.should_return_null else
      List(elements).set_contexto(contexto).set_pos(node.posicion_inicial, node.posicion_final)
    )

  def recorre_FuncDefNode(self, node, contexto):
    res = RTresultado()

    func_name = node.var_name_tok.value if node.var_name_tok else None
    body_node = node.body_node
    arg_names = [arg_name.value for arg_name in node.arg_name_toks]
    func_value = Function(func_name, body_node, arg_names, node.should_auto_return).set_contexto(contexto).set_pos(node.posicion_inicial, node.posicion_final)

    if node.var_name_tok:
      contexto.symbol_table.set(func_name, func_value)

    return res.success(func_value)

  def recorre_CallNode(self, node, contexto):
    res = RTresultado()
    args = []

    value_to_call = res.registrar(self.recorre(node.node_to_call, contexto))
    if res.should_return(): return res
    value_to_call = value_to_call.copy().set_pos(node.posicion_inicial, node.posicion_final)

    for arg_node in node.arg_nodes:
      args.append(res.registrar(self.recorre(arg_node, contexto)))
      if res.should_return(): return res

    return_value = res.registrar(value_to_call.execute(args))
    if res.should_return(): return res
    return_value = return_value.copy().set_pos(node.posicion_inicial, node.posicion_final).set_contexto(contexto)
    return res.success(return_value)

  def recorre_ReturnNode(self, node, contexto):
    res = RTresultado()

    if node.node_to_return:
      value = res.registrar(self.recorre(node.node_to_return, contexto))
      if res.should_return(): return res
    else:
      value = Number.null

    return res.success_return(value)

  def recorre_ContinueNode(self, node, contexto):
    return RTresultado().success_continue()

  def recorre_BreakNode(self, node, contexto):
    return RTresultado().success_break()

#######################################
# RUN
#######################################

tabla_simbolos_global = SymbolTable()
tabla_simbolos_global.set("NULL", Number.null)
tabla_simbolos_global.set("FALSE", Number.false)
tabla_simbolos_global.set("TRUE", Number.true)
tabla_simbolos_global.set("MATH_PI", Number.math_PI)
tabla_simbolos_global.set("PRESI", FuncionBuiltIn.print)
tabla_simbolos_global.set("PRINT_RET", FuncionBuiltIn.print_ret)
tabla_simbolos_global.set("INPUT", FuncionBuiltIn.input)
tabla_simbolos_global.set("INPUT_INT", FuncionBuiltIn.input_int)
tabla_simbolos_global.set("IS_NUM", FuncionBuiltIn.is_number)
tabla_simbolos_global.set("IS_STR", FuncionBuiltIn.is_string)
tabla_simbolos_global.set("IS_LIST", FuncionBuiltIn.is_list)
tabla_simbolos_global.set("IS_FUN", FuncionBuiltIn.is_function)
tabla_simbolos_global.set("APPEND", FuncionBuiltIn.append)
tabla_simbolos_global.set("EXTEND", FuncionBuiltIn.extend)
tabla_simbolos_global.set("LONGECO", FuncionBuiltIn.len)
tabla_simbolos_global.set("RUN", FuncionBuiltIn.run)

def run(nombre_archivo, text):
  # Generate tokens
  lexer = Lexer(nombre_archivo, text)
  tokens, error = lexer.make_tokens()
  if error: return None, error

  # Generate AST
  parser = Parser(tokens)
  ast = parser.parse()
  if ast.error: return None, ast.error

  # Run program
  semantico = Semantico()
  contexto = Context('<program>')
  contexto.symbol_table = tabla_simbolos_global
  resultado = semantico.recorre(ast.node, contexto)

  return resultado.value, resultado.error
