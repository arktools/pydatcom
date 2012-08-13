#!/usr/bin/env python

import sys
import ply.lex as lex
import ply.yacc as yacc
import os

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
        while 1:
            try:
                s = raw_input('datcom > ')
            except EOFError:
                break
            if not s: continue     
            yacc.parse(s)

    
class Calc(Parser):

    states = (
        ('INPUT','exclusive'),
        ('OUTPUT','exclusive'),
    )

    reserved = {
        'exit' : 'EXIT',
    }

    tokens = [
        'NAME','NUMBER', 'EQUALS',
        'DELIM','COMMA',
        ] + reserved.values()

    # Tokens
    t_INPUT_EQUALS  = r'='
    t_INPUT_DELIM = '\$'
    t_INPUT_NAME = r'[a-zA-Z_][a-zA-Z0-9_]*'
    t_ANY_ignore = '\t'

    def t_NUMBER(self, t):
        r'\d+'
        try:
            t.value = int(t.value)
        except ValueError:
            print("Integer value too large %s" % t.value)
            t.value = 0
        #print "parsed number %s" % repr(t.value)
        return t

    def t_ANY_newline(self, t):
        r'\n+'
        t.lexer.lineno += t.value.count("\n")
    
    def t_ANY_error(self, t):
        print("Illegal character '%s'" % t.value[0])
        t.lexer.skip(1)

    def t_INPUT_end_INPUT(self, t):
        r'end\ input'
        t.lexer.begin('INITIAL')

    def t_INITIAL_begin_INPUT(self, t):
        r'input'
        t.lexer.begin('INPUT')

    # Parsing rules

    precedence = ()

    def p_assign(self, p):
        """
        expression : NAME EQUALS NUMBER
        """
        p[0] = {p[1], p[3]}

    def p_expression_list(self, p):
        """
        expressions : expression COMMA expresssions
        """
        p[0] = p[3]
        for key in p[1].keys():
            if key in p[0]:
                raise KeyError('duplicate key %s' % key)
            else:
                p[0][key] = p[1][key]

    def p_expressions(self, p):
        """
        expressions : expression
        """
        p[0] = p[3]

    def p_assignments(self, p):
        """
        statement : DELIM NAME expressions DELIM 
        """
        self.data[p[2]] = p[3]

    def p_exit(self, p):
        """
        statement : EXIT
        """
        raise SystemExit('exitting')

    def p_error(self, p):
        if p:
            print("Syntax error at '%s'" % p.value)
        else:
            print("Syntax error at EOF")

if __name__ == '__main__':
    calc = Calc()
    calc.run()
