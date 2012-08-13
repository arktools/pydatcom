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

    
class DatcomParser(Parser):

    states = (
        ('INPUT','exclusive'),
    )

    reserved = {
        'exit' : 'EXIT',
    }

    tokens = [
        'NAME',
        'NUMBER',
        'EQUALS',
        'DELIM', # delimeter
        'COMMA',
        'WS', # whitespace
        ] + reserved.values()

    # Tokens
    t_ANY_COMMA = ','

    t_INITIAL_ignore = ' \t'

    t_INPUT_EQUALS  = r'='
    t_INPUT_DELIM = '\$'
    t_INPUT_WS = '[ \t\r\n]+'

    t_INPUT_ignore = ''


    def t_ANY_newline(self, t):
        r'\n+'
        t.lexer.lineno += t.value.count("\n")
    
    def t_ANY_error(self, t):
        print("Illegal character '%s'" % t.value[0])
        t.lexer.skip(1)

    def t_INPUT_end_INPUT(self, t):
        r'end\ input'
        print 'ending input'
        t.lexer.begin('INITIAL')

    def t_INITIAL_begin_INPUT(self, t):
        r'input'
        print 'begin input'
        t.lexer.begin('INPUT')

    def t_ANY_NAME(self, t):
        r'[a-zA-Z_][a-zA-Z0-9_]*'
        t.type = self.reserved.get(t.value,'NAME')
        return t

    def t_ANY_NUMBER(self, t):
        r'\d+'
        try:
            t.value = float(t.value)
        except ValueError:
            print("Could not create float from %s" % t.value)
            t.value = 0
        return t

    # Parsing rules

    #precedence = ()

    def p_assign(self, p):
        """
        expression : NAME WS EQUALS WS NUMBER
        """
        p[0] = {p[1], p[5]}
        print p[0]

    def p_expression_list(self, p):
        """
        expression : expression WS COMMA WS expression
        """
        p[0] = p[5]
        for key in p[1].keys():
            if key in p[0]:
                raise KeyError('duplicate key %s' % key)
            else:
                p[0][key] = p[1][key]
        print p

    def p_statement_assignments(self, p):
        """
        statement : DELIM NAME WS expression WS DELIM 
        """
        self.data[p[2]] = p[4]

    def p_statement_exit(self, p):
        """
        statement : EXIT
        """
        raise SystemExit('good bye')

    def p_error(self, p):
        if p:
            print("Syntax error at '%s'" % p.value)
        else:
            print("Syntax error at EOF")

if __name__ == '__main__':
    parser = DatcomParser()
    parser.run()
