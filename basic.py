#######################################
# IMPORTS
#######################################

#from cgitb import text
#from turtle import left, right
#from xml.dom.minicompat import NodeList
from strings_with_arrows import *
import string

#######################################
# CONSTANTS
#######################################

DIGITS = '0123456789'
LETTERS = string.ascii_letters
LETTERS_DIGITS = LETTERS + DIGITS

#######################################
# ERRORS
#######################################

class Error:
	def __init__(self, pos_start, pos_end, error_name, details):
		self.pos_start = pos_start
		self.pos_end = pos_end
		self.error_name = error_name
		self.details = details
	
	def as_string(self):
		result  = f'{self.error_name}: {self.details}\n'
		result += f'File {self.pos_start.fn}, line {self.pos_start.ln + 1}'
		result += '\n\n' + string_with_arrows(self.pos_start.ftxt, self.pos_start, self.pos_end)
		return result

class IllegalCharError(Error):
	def __init__(self, pos_start, pos_end, details):
		super().__init__(pos_start, pos_end, 'Illegal Character', details)

class ExpectedCharError(Error):
    	def __init__(self, pos_start, pos_end, details):
		    super().__init__(pos_start, pos_end, 'Expected Character', details)
  
class InvalidSyntaxError(Error):
	def __init__(self, pos_start, pos_end, details=''):
		super().__init__(pos_start, pos_end, 'Invalid Syntax', details)

#ver si cambiamos end por final y start con inicio
class RTError(Error):
	def __init__(self, pos_start, pos_end, details, contexto):
		super().__init__(pos_start, pos_end, 'Runtime Error', details)
		self.contexto = contexto
	
	def as_string(self):
		result  = self.generate_traceback()
		result += f'{self.error_name}: {self.details}'
		result += '\n\n' + string_with_arrows(self.pos_start.ftxt, self.pos_start, self.pos_end)
		return result
	
	def generate_traceback(self):
		result = ''
		pos = self.pos_start
		ctx = self.contexto
		
		while ctx:
			#el resultado q mostrara
			result = f'  File {pos.fn}, line {str(pos.ln + 1)}, in {ctx.nombre_display}\n' + result
			pos = ctx.parent_pos_entrada
			ctx = ctx.parent
			

		return 'Traceback (most recent call last):\n' + result
	
#######################################
# POSITION
#######################################

class Position:
	def __init__(self, idx, ln, col, fn, ftxt):
		self.idx = idx
		self.ln = ln
		self.col = col
		self.fn = fn
		self.ftxt = ftxt

	def advance(self, current_char=None):
		self.idx += 1
		self.col += 1

		if current_char == '\n':
			self.ln += 1
			self.col = 0

		return self

	def copy(self):
		return Position(self.idx, self.ln, self.col, self.fn, self.ftxt)

#######################################
# TOKENS
#######################################

TT_INT		= 'INT'
TT_FLOAT    = 'FLOAT'
TT_STRING	= 'STRING'
TT_IDENTIFIER	= 'IDENTIFIER'
TT_KEYWORD		= 'KEYWORD'
TT_PLUS     = 'PLUS'
TT_MINUS    = 'MINUS'
TT_MUL      = 'MUL'
TT_DIV      = 'DIV'
TT_POW		= 'POW'
TT_EQ		= 'EQ'
TT_LPAREN   = 'LPAREN'
TT_RPAREN   = 'RPAREN'
TT_MDKRAMPO = 'MDKRAMPO' #(Corchete izquierdo)
TT_DKRAMPO  = 'DKRAMPO'  #(Corchete derecho)
TT_EE		= 'EE'
TT_NE		= 'NE'
TT_LT		= 'LT'
TT_GT		= 'GT'
TT_LTE		= 'LTE'
TT_GTE		= 'GTE'
TT_COMMA	= 'COMMA'
TT_ARROW	= 'ARROW'
TT_EOF		= 'EOF'

KEYWORDS = [
	'VAR',
	'AND',
	'OR',
	'NOT',
	'IF',
	'ELIF',
	'ELSE',
	'FOR',
	'TO',
	'STEP',
	'WHILE',
	'FUN',
	'THEN'
]

class Token:
    def __init__(self, type_, value=None, pos_start=None, pos_end=None):
        self.type = type_
        self.value = value
        
        if pos_start:
            self.pos_start = pos_start.copy()
            self.pos_end = pos_start.copy()
            self.pos_end.advance()
            
        if pos_end:
            self.pos_end = pos_end.copy()
   
    def matches(self, type_, value):
        return self.type == type_ and self.value == value
    
    def __repr__(self):
        if self.value: return f'{self.type}:{self.value}'
        return f'{self.type}'

#######################################
# LEXER
#######################################

class Lexer:
    def __init__(self, fn, text):
        self.fn = fn
        self.text = text
        self.pos = Position(-1, 0, -1, fn, text)
        self.current_char = None
        self.advance()
        
    def advance(self):
        self.pos.advance(self.current_char)
        self.current_char = self.text[self.pos.idx] if self.pos.idx < len(self.text) else None
    
    def make_tokens(self):
        tokens = []
        
        while self.current_char != None:
            if self.current_char in ' \t':
                self.advance()
            elif self.current_char in DIGITS:
                tokens.append(self.make_number())
            elif self.current_char in LETTERS:
                tokens.append(self.make_identifier())
            elif self.current_char == '"':
                tokens.append(self.make_string())     
            elif self.current_char == '+':
                tokens.append(Token(TT_PLUS, pos_start=self.pos))
                self.advance()
            elif self.current_char == '-':
                tokens.append(Token(TT_MINUS, pos_start=self.pos))
                self.advance()
            elif self.current_char == '*':
                tokens.append(Token(TT_MUL, pos_start=self.pos))
                self.advance()
            elif self.current_char == '/':
                tokens.append(Token(TT_DIV, pos_start=self.pos))
                self.advance()
            elif self.current_char == '(':
                tokens.append(Token(TT_LPAREN, pos_start=self.pos))
                self.advance()
            elif self.current_char == ')':
                tokens.append(Token(TT_RPAREN, pos_start=self.pos))
                self.advance()
            elif self.current_char == '[':
                tokens.append(Token(TT_MDKRAMPO, pos_start=self.pos))
                self.advance()
            elif self.current_char == ']':
                tokens.append(Token(TT_DKRAMPO, pos_start=self.pos))
                self.advance()
            elif self.current_char == '!':
                token, error = self.make_not_equals()
                if error: return [], error
                tokens.append(token)
            elif self.current_char == '=':
                tokens.append(self.make_equals())
            elif self.current_char == '<':
                tokens.append(self.make_less_than())
            elif self.current_char == '>':
                tokens.append(self.make_greater_than())
            elif self.current_char == ',':
                tokens.append(Token(TT_COMMA, pos_start=self.pos))
                self.advance()
            else:
                pos_start = self.pos.copy()
                char = self.current_char
                self.advance()
                return [], IllegalCharError(pos_start, self.pos, "'" + char + "'")
            
        tokens.append(Token(TT_EOF, pos_start=self.pos))
        return tokens, None
    
    def make_number(self):
        num_str = ''
        dot_count = 0
        pos_start = self.pos.copy()
        
        while self.current_char != None and self.current_char in DIGITS + '.':
            if self.current_char == '.':
                if dot_count == 1: break
                dot_count += 1
                num_str += '.'
            else:
                num_str += self.current_char
            self.advance()
        
        if dot_count == 0:
            return Token(TT_INT, int(num_str), pos_start, self.pos)
        else:
            return Token(TT_FLOAT, float(num_str), pos_start, self.pos)
        
    def make_string(self):
        string = ''
        pos_start = self.pos.copy()
        escape_character = False
        self.advance()
        
        escape_characters = {
			'n': '\n',
			't': '\t'
		}
        
        while self.current_char != None and (self.current_char != '"' or escape_character):
            if escape_character:
                string += escape_characters.get(self.current_char, self.current_char)
            else:
                if self.current_char == '\\':
                    escape_character = True
                else:
                    string += self.current_char
            self.advance()
            escape_character = False
        
        self.advance()
        return Token(TT_STRING, string, pos_start, self.pos)
    
    def make_identifier(self):
        id_str = ''
        pos_start = self.pos.copy()
        
        while self.current_char != None and self.current_char in LETTERS_DIGITS + '_':
            id_str += self.current_char
            self.advance()
            
        tok_type = TT_KEYWORD if id_str in KEYWORDS else TT_IDENTIFIER
        return Token(tok_type, id_str, pos_start, self.pos)
    
    def make_minus_or_arrow(self):
        tok_type = TT_MINUS
        pos_start = self.pos.copy()
        self.advance()
        
        if self.current_char == '>':
            self.advance()
            tok_type = TT_ARROW
            
        return Token(tok_type, pos_start=pos_start, pos_end=self.pos)
    
    def make_not_equals(self):
        pos_start = self.pos.copy()
        self.advance()
        
        if self.current_char == '=':
            self.advance()
            return Token(TT_NE, pos_start=pos_start, pos_end=self.pos), None
        
        self.advance()
        return None, ExpectedCharError(pos_start, self.pos, "'=' (after '!')")
    
    def make_equals(self):
        tok_type = TT_EQ
        pos_start = self.pos.copy()
        self.advance()
        
        if self.current_char == '=':
            self.advance()
            tok_type = TT_EE
            
        return Token(tok_type, pos_start=pos_start, pos_end=self.pos)
    
    def make_less_than(self):
        tok_type = TT_LT
        pos_start = self.pos.copy()
        self.advance()
        
        if self.current_char == '=':
            self.advance()
            tok_type = TT_LTE
            
        return Token(tok_type, pos_start=pos_start, pos_end=self.pos)
    
    def make_greater_than(self):
        tok_type = TT_GT
        pos_start = self.pos.copy()
        self.advance()
        
        if self.current_char == '=':
            self.advance()
            tok_type = TT_GTE
            
        return Token(tok_type, pos_start=pos_start, pos_end=self.pos)
    
#######################################
# NODES
#######################################

class NumberNode:
	def __init__(self, tok):
		self.tok = tok

		self.pos_start = self.tok.pos_start
		self.pos_end = self.tok.pos_end

	def __repr__(self):
		return f'{self.tok}'

class StringNode:
    	def __init__(self, tok):
            self.tok = tok
      
            self.pos_start = self.tok.pos_start
            self.pos_end = self.tok.pos_end
            
        def __repr__(self):
            return f'{self.tok}'

class NodoLista:
    def __init__(self, nodos_elementos, pos_start, pos_end):
        self.nodos_elementos = nodos_elementos
        self.pos_start = pos_start
        self.pos_end = pos_end

class VarAccessNode:
    def __init__(self, var_name_tok):
        self.var_name_tok = var_name_tok

        self.pos_start = self.var_name_tok.pos_start
        self.pos_end = self.var_name_tok.pos_end

class VarAssignNode:
    def __init__(self, var_name_tok, value_node):
        self.var_name_tok = var_name_tok
        self.value_node = value_node

        self.pos_start = self.var_name_tok.pos_start
        self.pos_end = self.value_node.pos_end
    
class BinOpNode:
	def __init__(self, left_node, op_tok, right_node):
		self.left_node = left_node
		self.op_tok = op_tok
		self.right_node = right_node

		self.pos_start = self.left_node.pos_start
		self.pos_end = self.right_node.pos_end

	def __repr__(self):
		return f'({self.left_node}, {self.op_tok}, {self.right_node})'

class UnaryOpNode:
	def __init__(self, op_tok, node):
		self.op_tok = op_tok
		self.node = node

		self.pos_start = self.op_tok.pos_start
		self.pos_end = node.pos_end

	def __repr__(self):
		return f'({self.op_tok}, {self.node})'

class IfNode:
    def __init__(self, cases, else_case):
        self.cases = cases
        self.else_case = else_case
        
        self.pos_start = self.cases[0][0].pos_start
        self.pos_end = (self.else_case or self.cases[len(self.cases) - 1][0]).pos_end

class ForNode:
	def __init__(self, var_name_tok, start_value_node, end_value_node, step_value_node, body_node):
		self.var_name_tok = var_name_tok
		self.start_value_node = start_value_node
		self.end_value_node = end_value_node
		self.step_value_node = step_value_node
		self.body_node = body_node

		self.pos_start = self.var_name_tok.pos_start
		self.pos_end = self.body_node.pos_end

class WhileNode:
	def __init__(self, condition_node, body_node):
		self.condition_node = condition_node
		self.body_node = body_node

		self.pos_start = self.condition_node.pos_start
		self.pos_end = self.body_node.pos_end

class FuncDefNode:
	def __init__(self, var_name_tok, arg_name_toks, body_node):
		self.var_name_tok = var_name_tok
		self.arg_name_toks = arg_name_toks
		self.body_node = body_node

		if self.var_name_tok:
			self.pos_start = self.var_name_tok.pos_start
		elif len(self.arg_name_toks) > 0:
			self.pos_start = self.arg_name_toks[0].pos_start
		else:
			self.pos_start = self.body_node.pos_start

		self.pos_end = self.body_node.pos_end

class CallNode:
	def __init__(self, node_to_call, arg_nodes):
		self.node_to_call = node_to_call
		self.arg_nodes = arg_nodes

		self.pos_start = self.node_to_call.pos_start

		if len(self.arg_nodes) > 0:
			self.pos_end = self.arg_nodes[len(self.arg_nodes) - 1].pos_end
		else:
			self.pos_end = self.node_to_call.pos_end

#######################################
# PARSE RESULT
#######################################

class ParseResult:
	def __init__(self):
		self.error = None
		self.node = None
		self.last_registered_advance_count = 0
		self.advance_count = 0

	def register_advancement(self):
		self.last_registered_advance_count = 1
		self.advance_count += 1

	def register(self, res):
		self.last_registered_advance_count = res.advance_count
		self.advance_count += res.advance_count
		if res.error: self.error = res.error
		return res.node

	def success(self, node):
		self.node = node
		return self

	def failure(self, error):
		if not self.error or self.last_registered_advance_count == 0:
			self.error = error
		return self

#######################################
# PARSER
#######################################

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.tok_idx = -1
        self.advance()
    
    def advance(self, ):
        self.tok_idx += 1
        if self.tok_idx < len(self.tokens):
            self.current_tok = self.tokens[self.tok_idx]
        return self.current_tok
    
    def parse(self):
        res = self.expr()
        if not res.error and self.current_tok.type != TT_EOF:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected '+', '-', '*', '/', '^', '==', '!=', '<', '>', <=', '>=', 'AND' or 'OR'"
            ))
        return res
    
    ##################################
    
    def expr(self):
        res = ParseResult()
        
        if self.current_tok.matches(TT_KEYWORD, 'VAR'):
            res.register_advancement()
            self.advance()
            
            if self.current_tok.type != TT_IDENTIFIER:
                return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Expected identifier"
				))
            
            var_name = self.current_tok
            res.register_advancement()
            self.advance()
            
            if self.current_tok.type != TT_EQ:
                return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Expected '='"
				))
            
            res.register_advancement()
            self.advance()
            expr = res.register(self.expr())
            if res.error: return res
            return res.success(VarAssignNode(var_name, expr))
        
        node = res.register(self.bin_op(self.comp_expr, ((TT_KEYWORD, 'AND'), (TT_KEYWORD, 'OR'))))
        
        if res.error:
            return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"Expected 'VAR', 'IF', 'FOR', 'WHILE', 'FUN', int, float, identifier, '+', '-', '(', '[' or 'NOT'"
			))
        
        return res.success(node)
    
    def comp_expr(self):
        res = ParseResult()
        
        if self.current_tok.matches(TT_KEYWORD, 'NOT'):
            op_tok = self.current_tok
            res.register_advancement()
            self.advance()
            
            node = res.register(self.comp_expr())
            if res.error: return res
            return res.success(UnaryOpNode(op_tok, node))
        
        node = res.register(self.bin_op(self.arith_expr, (TT_EE, TT_NE, TT_LT, TT_GT, TT_LTE, TT_GTE)))
        
        if res.error:
            return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"Expected int, float, identifier, '+', '-', '(', '['or 'NOT'"
			))
            
        return res.success(node)
    
    def arith_expr(self):
        return self.bin_op(self.term, (TT_PLUS, TT_MINUS))
    
    def term(self):
        return self.bin_op(self.factor, (TT_MUL, TT_DIV))
    
    def factor(self):
        res = ParseResult()
        tok = self.current_tok
        
        if tok.type in (TT_PLUS, TT_MINUS):
            res.register_advancement()
            self.advance()
            factor = res.register(self.factor())
            if res.error: return res
            return res.success(UnaryOpNode(tok, factor))
        
        return self.power()
    
    def power(self):
        return self.bin_op(self.call, (TT_POW, ), self.factor)
    
    def call(self):
        res = ParseResult()
        atom = res.register(self.atom())
        if res.error: return res
        
        if self.current_tok.type == TT_LPAREN:
            res.register_advancement()
            self.advance()
            arg_nodes = []
            
            if self.current_tok.type == TT_RPAREN:
                res.register_advancement()
                self.advance() 
            else:
                arg_nodes.append(res.register(self.expr()))
                if res.error:
                    return res.failure(InvalidSyntaxError(
						self.current_tok.pos_start, self.current_tok.pos_end,
						"Expected ')', 'VAR', 'IF', 'FOR', 'WHILE', 'FUN', int, float, identifier, '+', '-', '(', '[' or 'NOT'"
					))
                
                while self.current_tok.type == TT_COMMA:
                    res.register_advancement()
                    self.advance()
                    
                    arg_nodes.append(res.register(self.expr()))
                    if res.error: return res
                    
                if self.current_tok.type != TT_RPAREN:
                    return res.failure(InvalidSyntaxError(
						self.current_tok.pos_start, self.current_tok.pos_end,
						f"Expected ',' or ')'"
					))
                    
                res.register_advancement()
                self.advance()
            return res.success(CallNode(atom, arg_nodes))
        return res.success(atom)

    def atom(self):
        res = ParseResult()
        tok = self.current_tok
        
        if tok.type in (TT_INT, TT_FLOAT):
            res.register_advancement()
            self.advance()
            return res.success(NumberNode(tok))
        
        elif tok.type == TT_STRING:
            res.register_advancement()
            self.advance()
            return res.success(StringNode(tok))
        
        elif tok.type == TT_IDENTIFIER:
            res.register_advancement()
            self.advance()
            return res.success(VarAccessNode(tok))
        
        #para el corchete de apertura
        elif tok.type == TT_LPAREN:
            res.register_advancement()
            self.advance()
            expr = res.register(self.expr())
            if res.error: return res
            if self.current_tok.type == TT_RPAREN:
                res.register_advancement()
                self.advance()
                return res.success(expr)
            else:
                return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Expected ')'"
				))
        #para el corchete de cerradura        
        elif tok.type == TT_DKRAMPO:
            list_expr = res.register(self.list_expr)
            if res.error: return res
            return res.success(list_expr)
                
        elif tok.matches(TT_KEYWORD, 'IF'):
            if_expr = res.register(self.if_expr())
            if res.error: return res
            return res.success(if_expr)
        
        elif tok.matches(TT_KEYWORD, 'FOR'):
            for_expr = res.register(self.for_expr())
            if res.error: return res
            return res.success(for_expr)
        
        elif tok.matches(TT_KEYWORD, 'WHILE'):
            while_expr = res.register(self.while_expr())
            if res.error: return res
            return res.success(while_expr)
        
        elif tok.matches(TT_KEYWORD, 'FUN'):
            func_def = res.register(self.func_def())
            if res.error: return res
            return res.success(func_def)
        
        return res.failure(InvalidSyntaxError(
			tok.pos_start, tok.pos_end,
			"Expected int, float, identifier, '+', '-', '(', '[', 'IF', 'FOR', 'WHILE', 'FUN'"
		))
        
    def list_expr(self):
        res = ParseResult()
        nodos_elementos = []
        pos_start = self.current_tok.pos_start.copy()
        
        #cuando no hay el corchete izquierdo
        if self.current_tok.type != TT_MDKRAMPO:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
                f"Expected '['"
            ))
        
        res.register_advancement()
        self.advance()
        #ahora si ya encontramos el corchete izquierdo, 
        #debemos seguir analizando lo q sigue
        
        #para lista vacia, q solo contenga "[]"
        if self.current_tok.type == TT_DKRAMPO:
            res.register_advancement()
            self.advance()
        else:
            nodos_elementos.append(res.register(self.expr()))
            if res.error:
                return res.failure(InvalidSyntaxError(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Expected ']', 'VAR', 'IF', 'FOR', 'WHILE', 'FUN', int, float, identifier, '+', '-', '(', '[' or 'NOT'"
                ))

            while self.current_tok.type == TT_COMMA:
                res.register_advancement()
                self.advance()

                nodos_elementos.append(res.register(self.expr()))
            if res.error: return res

        if self.current_tok.type != TT_DKRAMPO:
            return res.failure(InvalidSyntaxError(
                self.current_tok.pos_start, self.current_tok.pos_end,
               f"Expected ',' or '['"
        ))

        res.register_advancement()
        self.advance()

        return res.success(NodoLista(
        nodos_elementos,
         pos_start,
        self.current_tok.pos_end.copy()
        ))
        
    def if_expr(self):
        res = ParseResult()
        cases = []
        else_case = None
        
        if not self.current_tok.matches(TT_KEYWORD, 'IF'):
            return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected 'IF'"
			))
            
        res.register_advancement()
        self.advance()
        
        condition = res.register(self.expr())
        if res.error: return res
        
        if not self.current_tok.matches(TT_KEYWORD, 'THEN'):
            return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected 'THEN'"
			))
            
        res.register_advancement()
        self.advance()
        
        expr = res.register(self.expr())
        if res.error: return res
        cases.append((condition, expr))
        
        while self.current_tok.matches(TT_KEYWORD, 'ELIF'):
            res.register_advancement()
            self.advance()
            
            condition = res.register(self.expr())
            if res.error: return res
            
            if not self.current_tok.matches(TT_KEYWORD, 'THEN'):
                return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					f"Expected 'THEN'"
				))
            
            res.register_advancement()
            self.advance()
            
            expr = res.register(self.expr())
            if res.error: return res
            cases.append((condition, expr))
            
            if self.current_tok.matches(TT_KEYWORD, 'ELSE'):
                res.register_advancement()
                self.advance()
                
                else_case = res.register(self.expr())
                if res.error: return res
                
            return res.success(IfNode(cases, else_case))
        
    def for_expr(self):
        res = ParseResult()
            
        if not self.current_tok.matches(TT_KEYWORD, 'FOR'):
            return res.failure(InvalidSyntaxError(
			self.current_tok.pos_start, self.current_tok.pos_end,
			f"Expected 'FOR'"
			))
            
        res.register_advancement()
        self.advance()
            
        if self.current_tok.type != TT_IDENTIFIER:
            return res.failure(InvalidSyntaxError(
			self.current_tok.pos_start, self.current_tok.pos_end,
			"Expected identifier"
		    ))
                
        var_name = self.current_tok
        res.register_advancement()
        self.advance()
            
        if self.current_tok.type != TT_EQ:
            return res.failure(InvalidSyntaxError(
		    self.current_tok.pos_start, self.current_tok.pos_end,
			f"Expected '='"
			))
                
        res.register_advancement()
        self.advance()
            
        start_value = res.register(self.expr())
        if res.error: return res
            
        if not self.current_tok.matches(TT_KEYWORD, 'TO'):
            return res.failure(InvalidSyntaxError(
		    self.current_tok.pos_start, self.current_tok.pos_end,
			f"Expected 'TO'"
		    ))
            
        res.register_advancement()
        self.advance()
            
        end_value = res.register(self.expr())
        if res.error: return res
        
        if self.current_tok.matches(TT_KEYWORD, 'STEP'):
            res.register_advancement()
            self.advance()
            
            step_value = res.register(self.expr())
            if res.error: return res
        else:
            step_value = None
            
        if not self.current_tok.matches(TT_KEYWORD, 'THEN'):
            return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected 'THEN'"
			))
            
        res.register_advancement()
        self.advance()
        
        body = res.register(self.expr())
        if res.error: return res
        
        return res.success(ForNode(var_name, start_value, end_value, step_value, body))
    
    def while_expr(self):
        res = ParseResult()
        
        if not self.current_tok.matches(TT_KEYWORD, 'WHILE'):
            return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected 'WHILE'"
			))
        
        res.register_advancement()
        self.advance()
        
        condition = res.register(self.expr())
        if res.error: return res
        
        if not self.current_tok.matches(TT_KEYWORD, 'THEN'):
            return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected 'THEN'"
			))
            
        res.register_advancement()
        self.advance()
        
        body = res.register(self.expr())
        if res.error: return res
        
        return res.success(WhileNode(condition, body))
    
    def func_def(self):
        res = ParseResult()
        if not self.current_tok.matches(TT_KEYWORD, 'FUN'):
            return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected 'FUN'"
			))
        
        res.register_advancement()
        self.advance()
        
        if self.current_tok.type == TT_IDENTIFIER:
            var_name_tok = self.current_tok
            res.register_advancement()
            self.advance()
            if self.current_tok.type != TT_LPAREN:
                return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					f"Expected '('"
				))   
        else:
            var_name_tok = None
            if self.current_tok.type != TT_LPAREN:
                return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					f"Expected identifier or '('"
				))
                
        res.register_advancement()
        self.advance()
        arg_name_toks = []
        
        if self.current_tok.type == TT_IDENTIFIER:
            arg_name_toks.append(self.current_tok)
            res.register_advancement()
            self.advance()
            
            while self.current_tok.type == TT_COMMA:
                res.register_advancement()
                self.advance()
                
                if self.current_tok.type != TT_IDENTIFIER:
                    return res.failure(InvalidSyntaxError(
						self.current_tok.pos_start, self.current_tok.pos_end,
						f"Expected identifier"
					))
                    
                arg_name_toks.append(self.current_tok)
                res.register_advancement()
                self.advance()
                
            if self.current_tok.type != TT_RPAREN:
                return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					f"Expected ',' or ')'"
				))
                
        else:
            if self.current_tok.type != TT_RPAREN:
                return res.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					f"Expected identifier or ')'"
				))
                
        res.register_advancement()
        self.advance()
        
        if self.current_tok.type != TT_ARROW:
            return res.failure(InvalidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected '->'"
			))
        res.register_advancement()
        self.advance()
        node_to_return = res.register(self.expr())
        if res.error: return res
        
        return res.success(FuncDefNode(
			var_name_tok,
			arg_name_toks,
			node_to_return
		))
    
    #################################
    
    def bin_op(self, func_a, ops, func_b=None):
        if func_b == None:
            func_b = func_a
            
        res = ParseResult()
        left = res.register(func_a())
        if res.error: return res
        
        while self.current_tok.type in ops or (self.current_tok.type, self.current_tok.value) in ops:
            op_tok = self.current_tok
            res.register_advancement()
            self.advance()
            right = res.register(func_b())
            
            if res.error: return res
            left = BinOpNode(left, op_tok, right)
            
        return res.success(left)
    
#######################################
#  RESULTADOS EN TIEMPO DE EJECUCION  #
#######################################

class RTResult:
	def __init__(self):
		self.value = None
		self.error = None

	def register(self, res):
		if res.error: self.error = res.error
		return res.value
	#para el exito
	def success(self, value):
		self.value = value
		return self
	#para el fracaso
	def failure(self, error):
		self.error = error
		return self

#######################################
# VALUES
#######################################
class Value:
    def __init__(self):
        self.set_pos()
        self.set_context()
        
    def set_pos(self, pos_start=None, pos_end=None):
        self.pos_start = pos_start
        self.pos_end = pos_end
        
        return self
    
    def set_context(self, context=None):
        self.context = context
        return self
    
    def added_to(self, other):
        return None, self.illegal_operation(other)
    
    def subbed_by(self, other):
        return None, self.illegal_operation(other)
    
    def multed_by(self, other):
        return None, self.illegal_operation(other)
    
    def dived_by(self, other):
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
        return RTResult().failure(self.illegal_operation())
    
    def copy(self):
        raise Exception('No copy method defined')
    
    def is_true(self):
        return False
    
    def illegal_operation(self, other=None):
        if not other: other = self
        return RTError(
			self.pos_start, other.pos_end,
			'Illegal operation',
			self.context
		)

#esta clase es para q analice los caracteres numericos

class Number(Value):
    def __init__(self, value):
        self.value = value
		#self.set_pos()
		#self.set_context()
	#para que nos ayude a saber la posicion del error
    """def set_pos(self, pos_start=None, pos_end=None):
        self.pos_start = pos_start
        self.pos_end = pos_end
        return self
    
    def set_context(self, contexto=None):
        self.contexto = contexto
        return self
  	"""
	#para la suma
    def sumando_con(self, other):
        if isinstance(other, Number):
            return Number(self.value + other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)
        

	#para la resta
    def restando_con(self, other):
        if isinstance(other, Number):
            return Number(self.value - other.value).set_context(self.contexto), None
        else:
            return None, Value.illegal_operation(self, other)
        
	#para la multiplicacion
    def multiplicando_por(self, other):
        if isinstance(other, Number):
            return Number(self.value * other.value).set_context(self.contexto), None
        else:
            return None, Value.illegal_operation(self, other)
        
	#para la division
    def dividiendo_por(self, other):
        if isinstance(other, Number):
            if other.value == 0:
                return None, RTError(
					other.pos_start, other.pos_end,
					'Division by zero',
					self.contexto
				)
                
            return Number(self.value / other.value).set_context(self.contexto), None
        else:
            return None, Value.illegal_operation(self, other)
    
    def powed_by(self, other):
        if isinstance(other, Number):
            return Number(self.value ** other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)
        
    def get_comparison_eq(self, other):
        if isinstance(other, Number):
            return Number(int(self.value == other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)
        
    def get_comparison_ne(self, other):
        if isinstance(other, Number):
            return Number(int(self.value != other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)
        
    def get_comparison_lt(self, other):
        if isinstance(other, Number):
            return Number(int(self.value < other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)
    
    def get_comparison_gt(self, other):
        if isinstance(other, Number):
            return Number(int(self.value > other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)
        
    def get_comparison_lte(self, other):
        if isinstance(other, Number):
            return Number(int(self.value <= other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)
        
    def get_comparison_gte(self, other):
        if isinstance(other, Number):
            return Number(int(self.value >= other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)
        
    def anded_by(self, other):
        if isinstance(other, Number):
            return Number(int(self.value and other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)
        
    def ored_by(self, other):
        if isinstance(other, Number):
            return Number(int(self.value or other.value)).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)
    
    def notted(self):
        return Number(1 if self.value == 0 else 0).set_context(self.context), None
    
    def copy(self):
        copy = Number(self.value)
        copy.set_pos(self.pos_start, self.pos_end)
        copy.set_context(self.context)
        return copy
    
    def is_true(self):
        return self.value != 0
    
	#para representar el number
    def __repr__(self):
        return str(self.value)
    
class String(Value):
    def __init__(self, value):
        super().__init__()
        self.value = value
        
    def added_to(self, other):
        if isinstance(other, String):
            return String(self.value + other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)
        
    def multed_by(self, other):
        if isinstance(other, Number):
            return String(self.value * other.value).set_context(self.context), None
        else:
            return None, Value.illegal_operation(self, other)
    
    def is_true(self):
        return len(self.value) > 0
    
    def copy(self):
        copy = String(self.value)
        copy.set_pos(self.pos_start, self.pos_end)
        copy.set_context(self.context)
        return copy
    
    def __repr__(self):
        return f'"{self.value}"'

class Lista(Value):
    def __init__(self, elementos):
        super().__init__()
        self.elementos = elementos
    
    #para agregar un valor
    def sumando_con(self, other):
        nueva_lista = self.copy()
        nueva_lista.elementos.append(other)
        return nueva_lista, None
    
    #para quitar un elemento de la lista
    def restando_con(self, other):
        if isinstance(other, Number):
            nueva_lista = self.copy()
            try:
                nueva_lista.elementos.pop(other.value)
                return nueva_lista, None
            except:
                return None, RTError(
                    other.pos_start, other.pos_end,
                    'Element at this index could not be removed from list because index is out of bounds',
                    self.context
                )
                
        else: 
            return None, Value.illegal_operation(self, other)
    
    #para agregar otra lista
    def multiplicando_por(self, other):
        if isinstance(other, Lista):
            nueva_lista = self.copy()
            nueva_lista.elementos.extend(other.elementos)
            return nueva_lista, None
        else:
            return None, Value.illegal_operation(self, other)
        
    #para obtener un elemento de la lista
    def dividiendo_por(self, other):
        if isinstance(other, Number):
            try:
                return self.elementos[other.value], None
            except:
                return None, RTError(
                    other.pos_start, other.pos_end,
                    'Element at this index could not be retrieved from list because index is out of bounds',
                    self.context
                )
        else: 
            return None, Value.illegal_operation(self, other)
        
    #para realizar las copias
    def copy(self):
        copy = Lista(self.elementos[:])
        copy.set_pos(self.pos_start, self.pos_end)
        copy.set_context(self.context)
        return copy
    
    def __repr__(self):
        return f'[{", ".join([str(x) for x in self.elements])}]'
    
class Function(Value):
    def __init__(self, name, body_node, arg_names):
        super().__init__()
        self.name = name or "<anonymous>"
        self.body_node = body_node
        self.arg_names = arg_names
        
    def execute(self, args):
        res = RTResult()
        semantico = Semantico()
        new_context = Contexto(self.name, self.context, self.pos_start)
        new_context.symbol_table = SymbolTable(new_context.parent.symbol_table)
        
        if len(args) > len(self.arg_names):
            return res.failure(RTError(
                self.pos_start, self.pos_end,
                f"{len(args) - len(self.arg_names)} too many args passed into '{self.name}'",
                self.context
            ))
            
        if len(args) < len(self.arg_names):
            return res.failure(RTError(
                self.pos_start, self.pos_end,
                f"{len(self.arg_names) - len(args)} too few args passed into '{self.name}'",
                self.context
                ))
            
        for i in range(len(args)):
            arg_name = self.arg_names[i]
            arg_value = args[i]
            arg_value.set_context(new_context)
            new_context.symbol_table.set(arg_name, arg_value)
            
        value = res.register(semantico.visita(self.body_node, new_context))
        if res.error: return res
        return res.success(value)
    
    def copy(self):
        copy = Function(self.name, self.body_node, self.arg_names)
        copy.set_context(self.context)
        copy.set_pos(self.pos_start, self.pos_end)
        return copy
    
    def __repr__(self):
        return f"<function {self.name}>"
#######################################
# CONTEXTO
#######################################

class Contexto:
    def __init__(self, nombre_display, parent=None, parent_pos_entrada=None):
        self.nombre_display = nombre_display
        self.parent = parent
        self.parent_pos_entrada = parent_pos_entrada
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

#########################################
######### ANALIZADOR SINTACTICO #########
#########################################

class Semantico:
    #para recorrer los nodos
    def visita(self, node, contexto):
        nombre_metodo = f'visita_{type(node).__name__}' #cambiar luego
        metodo = getattr(self, nombre_metodo, self.metodo_no_visita)
        return metodo(node, contexto)
    
    #por si no tiene definido su method
    def metodo_no_visita(self, node, contexto):
        raise Exception(f'No visita_{type(node).__name__} method defined') #cambiar luego
    
    ########################
    #methods de visita para cada tipo de nodo
    def visita_NumberNode(self, node, contexto):
        #print("Nodo number encontrado!")
        return RTResult().success(
    		Number(node.tok.value).set_context(contexto).set_pos(node.pos_start, node.pos_end)
     	 )
    def visit_StringNode(self, node, context):
        return RTResult().success(
            String(node.tok.value).set_context(context).set_pos(node.pos_start, node.pos_end)
		) 
    #para cuando visitamos un nodo Lista
    def visita_NodoLista(self, node, contexto):
        res = RTResult()
        elementos = []
        
        for nodos_elementos in node.nodos_elementos:
            elementos.append(res.register(self.visita(nodos_elementos, contexto)))
            if res.error: return res
        return res.success(
            Lista(elementos).set_context(contexto).set_pos(node.pos_start, node.pos_end)
        )
    
    def visit_VarAccessNode(self, node, context):
        res = RTResult()
        var_name = node.var_name_tok.value
        value = context.symbol_table.get(var_name)
        
        if not value:
            return res.failure(RTError(
				node.pos_start, node.pos_end,
				f"'{var_name}' is not defined",
				context
			))
            
        value = value.copy().set_pos(node.pos_start, node.pos_end)
        return res.success(value)
    
    def visit_VarAssignNode(self, node, context):
        res = RTResult()
        var_name = node.var_name_tok.value
        value = res.register(self.visit(node.value_node, context))
        if res.error: return res
        
        context.symbol_table.set(var_name, value)
        return res.success(value)
    
    def visita_BinOpNode(self, node, contexto):
        #print("Nodo de Operador Binario encontrado!")
        res = RTResult()
        #como es binario, recorre a la derecha y a la izquierda
        left = res.register(self.visita(node.left_node, contexto))
        if res.error: return res
        right = res.register(self.visita(node.right_node, contexto))
        if res.error: return res
        #Trabaja con las diferentes operaciones (+,-,*,/)
        if node.op_tok.type == TT_PLUS:
            result, error = left.sumando_con(right)
        elif node.op_tok.type == TT_MINUS:
            result, error = left.restando_con(right)
        elif node.op_tok.type == TT_MUL:
            result, error = left.multiplicando_por(right)
        elif node.op_tok.type == TT_DIV:
            result, error = left.dividiendo_por(right)
        elif node.op_tok.type == TT_POW:
            result, error = left.powed_by(right)
        elif node.op_tok.type == TT_EE:
            result, error = left.get_comparison_eq(right)
        elif node.op_tok.type == TT_NE:
            result, error = left.get_comparison_ne(right)
        elif node.op_tok.type == TT_LT:
            result, error = left.get_comparison_lt(right)
        elif node.op_tok.type == TT_GT:
            result, error = left.get_comparison_gt(right)
        elif node.op_tok.type == TT_LTE:
            result, error = left.get_comparison_lte(right)
        elif node.op_tok.type == TT_GTE:
            result, error = left.get_comparison_gte(right)
        elif node.op_tok.matches(TT_KEYWORD, 'AND'):
            result, error = left.anded_by(right)
        elif node.op_tok.matches(TT_KEYWORD, 'OR'):
            result, error = left.ored_by(right)
        
        if error:
            return res.failure(error)
        else:
            return res.success(result.set_pos(node.pos_start, node.pos_end))
    
    def visita_UnaryOpNode(self, node, contexto):
        #print("Nodo Operador Unitario encontrado!")
        res = RTResult()
        #como es unitario, vuelve a si mismo
        number = res.register(self.visita(node.node, contexto))
        if res.error: return res
        
        error = None
        
        if node.op_tok.type == TT_MINUS:
            number, error = number.multiplicando_por(Number(-1))
            
        if error:
            return res.failure(error)
        else:
            return res.success(number.set_pos(node.pos_start, node.pos_end))
     
    def visit_IfNode(self, node, context):
        res = RTResult()
        
        for condition, expr in node.cases:
            condition_value = res.register(self.visit(condition, context))
            if res.error: return res
            
            if condition_value.is_true():
                expr_value = res.register(self.visit(expr, context))
                if res.error: return res
                return res.success(expr_value)
        
        if node.else_case:
            else_value = res.register(self.visit(node.else_case, context))
            if res.error: return res
            return res.success(else_value)
        
        return res.success(None)
    
    def visit_ForNode(self, node, context):
        res = RTResult()
        elementos = []
        
        start_value = res.register(self.visit(node.start_value_node, context))
        if res.error: return res
        
        end_value = res.register(self.visit(node.end_value_node, context))
        if res.error: return res
        
        if node.step_value_node:
            step_value = res.register(self.visit(node.step_value_node, context))
            if res.error: return res
        else:
            step_value = Number(1)
            
        i = start_value.value
        
        if step_value.value >= 0:
            condition = lambda: i < end_value.value
        else:
            condition = lambda: i > end_value.value
        
        while condition():
            context.symbol_table.set(node.var_name_tok.value, Number(i))
            i += step_value.value
            
            elementos.append(res.register(self.visit(node.body_node, context)))
            if res.error: return res
            
        return res.success(
            Lista(elementos).set_context(context).set_pos(node.pos_start, node.pos_end)
        )
    
    def visit_WhileNode(self, node, context):
        res = RTResult()
        elementos = []
        
        while True:
            condition = res.register(self.visit(node.condition_node, context))
            if res.error: return res
            
            if not condition.is_true(): break
            
            elementos.append(res.register(self.visit(node.body_node, context)))
            if res.error: return res
            
        return res.success(
            Lista(elementos).set_context(context).set_pos(node.pos_start, node.pos_end)
        )
    
    def visit_FuncDefNode(self, node, context):
        res = RTResult()
        
        func_name = node.var_name_tok.value if node.var_name_tok else None
        body_node = node.body_node
        arg_names = [arg_name.value for arg_name in node.arg_name_toks]
        func_value = Function(func_name, body_node, arg_names).set_context(context).set_pos(node.pos_start, node.pos_end)
        
        if node.var_name_tok:
            context.symbol_table.set(func_name, func_value)
            
        return res.success(func_value)
    
    def visit_CallNode(self, node, context):
        res = RTResult()
        args = []
        value_to_call = res.register(self.visit(node.node_to_call, context))
        if res.error: return res
        value_to_call = value_to_call.copy().set_pos(node.pos_start, node.pos_end)
        
        for arg_node in node.arg_nodes:
            args.append(res.register(self.visit(arg_node, context)))
            if res.error: return res
            
        return_value = res.register(value_to_call.execute(args))
        if res.error: return res
        return res.success(return_value)
        
##########################
# EJECUTAR #
###########################

global_symbol_table = SymbolTable()
global_symbol_table.set("NULL", Number(0))
global_symbol_table.set("FALSE", Number(0))
global_symbol_table.set("TRUE", Number(1))

def ejecutar(fn, text):
    # Generate tokens
   lexer = Lexer(fn, text)
   tokens, error = lexer.make_tokens()
   if error: return None, error
   
   # Generate AST
   parser = Parser(tokens)
   ast = parser.parse()
   if ast.error: return None, ast.error
   
   semantico = Semantico()
   contexto = Contexto('<program>')
   contexto.symbol_table = global_symbol_table
   result = semantico.visita(ast.node, contexto)
   
   return result.value, result.error
