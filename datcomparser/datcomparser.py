#!/usr/bin/env python

import sys
import ply.yacc as yacc
import ply.lex as lex
from ply.lex import TOKEN
import os
import string

class Parser(object):
    """
    Base class for a lexer/parser that has the rules defined as methods
    """
    tokens = ()
    precedence = ()

    def __init__(self, **kw):
        self.debug = kw.get('debug', 0)
        self.names = { }
        self.data = { }
        try:
            modname = os.path.split(os.path.splitext(__file__)[0])[1] + "_" + self.__class__.__name__
        except:
            modname = "parser"+"_"+self.__class__.__name__
        self.debugfile = modname + ".dbg"
        self.tabmodule = modname + "_" + "parsetab"
        #print self.debugfile, self.tabmodule

        # Build the lexer and parser
        lex.lex(module=self, debug=self.debug)
        yacc.yacc(module=self,
                  debug=self.debug,
                  debugfile=self.debugfile,
                  tabmodule=self.tabmodule)

    def run(self):
        if len(sys.argv) == 2:
            file_name = sys.argv[1]
            with open(file_name) as f:
                yacc.parse(f.read())
        else:
            while 1:
                try:
                    s = raw_input('datcom > ')
                except EOFError:
                    break
                if not s: continue     
                yacc.parse(s)

    
class DatcomParser(Parser):

    re_float = r'(\+|-)?\d+\.\d+(E(\+|-)?\d+)?'
    re_float_vector = r'({f})(\s*,\s*{f})*'.format(f=re_float)

    states = (
        ('INPUT','exclusive'),
    )

    reserved_ANY = {
        'exit' : 'EXIT',
    }

    reserved_INPUT = {
        'NACA' : 'NACA',
        'DIM'  : 'DIM',
        'DAMP'  : 'DAMP',
        'PART'  : 'PART',
        'DERIV' : 'DERIV',
    }

    tokens = [
        'NAME',
        'LPAREN',
        'RPAREN',
        'FLOATVECTOR',
        'INTEGER',
        'EQUALS',
        'DELIM', # delimeter
        'COMMA',
        'CASEID',
        'NEXTCASE',
        ] \
        + reserved_ANY.values() \
        + reserved_INPUT.values()

    # Tokens

    t_ANY_COMMA = ','
    t_ANY_LPAREN = '\('
    t_ANY_RPAREN = '\)'

    t_ANY_ignore = ' \t'

    t_INPUT_EQUALS  = r'='
    t_INPUT_DELIM = '\$'

    t_INPUT_ignore = ''


    def t_ANY_newline(self, t):
        r'\n+'
        t.lexer.lineno += t.value.count("\n")
    def t_ANY_error(self, t):
        #print("Illegal character '%s'" % t.value[0])
        t.lexer.skip(1)

    def t_INPUT_end_INPUT(self, t):
        r'^1.*AUTOMATED\ STABILITY\ AND\ CONTROL\ METHODS.*'
        print 'ending input'
        t.lexer.begin('INITIAL')

    def t_INITIAL_begin_INPUT(self, t):
        r'.*THE\ FOLLOWING\ IS'
        print 'begin input'
        t.lexer.begin('INPUT')

    def t_INPUT_NEXTCASE(self, t):
        r'.*NEXT\ CASE.*'

    def t_INPUT_CASEID(self, t):
        r'.*CASEID (.*)'
        print 'case: ', t.value

    def t_INPUT_NAME(self, t):
        r'[a-zA-Z_][a-zA-Z0-9_]*'
        t.type = self.reserved_ANY.get(t.value,'NAME')
        t.type = self.reserved_INPUT.get(t.value,'NAME')
        return t

    @TOKEN(re_float_vector)
    def t_INPUT_FLOATVECTOR(self, t):
        try:
            vector = []
            for num in string.split(t.value,','):
                vector.append(float(num)) 
            t.value = vector
        except ValueError:
            print("Could not create float from %s" % t.value)
            t.value = 0
        return t

    def t_ANY_INTEGER(self, t):
        r'\d+'
        try:
            t.value = int(t.value)
        except ValueError:
            print("Could not create int from %s" % t.value)
            t.value = 0
        return t

    # Parsing rules

    precedence = ()

    def p_exit(self, p):
        'statement : EXIT'
        raise SystemExit('good bye')

    def p_error(self, p):
        if p:
            print("Syntax error at '%s'" % p.value)
        else:
            print("Syntax error at EOF")

    def p_expression_assign(self, p):
        """
        expression : NAME EQUALS FLOATVECTOR
        """
        p[0] = {p[1] : p[3]}
        print p[0]

    def p_airfoil(self, p):
        """
        statement : NACA NAME INTEGER INTEGER
        """
        self.data[p[2]] = p[4]

    def p_dim(self, p):
        """
        statement : DIM NAME
        """
        self.data[p[1]] = p[2]

    def p_damp(self, p):
        """
        statement : DAMP
        """
        self.data[p[1]] = True

    def p_part(self, p):
        """
        statement : PART
        """
        self.data[p[1]] = True

    def p_deriv(self, p):
        """
        statement : DERIV NAME
        """
        self.data[p[1]] = p[2]

    def p_caseid(self, p):
        """
        statement : CASEID
        """
        self.data[p[1]] = p[1]
        print p[1]

    def p_expression_list(self, p):
        """
        expression : expression COMMA expression
        """
        p[0] = p[3]
        for key in p[1].keys():
            if key in p[0]:
                raise KeyError('duplicate key %s' % key)
            else:
                p[0][key] = p[1][key]

    def p_statement_assignments(self, p):
        """
        statement : DELIM NAME expression DELIM
        """
        self.data[p[2]] = p[3]

    def p_array_assign(self, p):
        """
        expression : NAME LPAREN INTEGER RPAREN EQUALS FLOATVECTOR 
        """
        p[0] = {p[1] : p[6]}
        print p[0]

if __name__ == '__main__':
    parser = DatcomParser()
    parser.run()
    print parser.data
