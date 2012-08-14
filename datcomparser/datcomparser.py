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

    def __init__(self,kw={}):
        self.debug = kw.get('debug', 0)
        self.file_name = kw.get('file_name', None)
        self.cases = []
        try:
            modname = os.path.split(os.path.splitext(__file__)[0])[1] + "_" + self.__class__.__name__
        except:
            modname = "parser"+"_"+self.__class__.__name__
        self.debugfile = modname + ".dbg"
        self.tabmodule = modname + "_" + "parsetab"
        #print self.debugfile, self.tabmodule

        # Build the lexer and parser
        self.lex = lex.lex(module=self, debug=self.debug)
        self.yacc = yacc.yacc(module=self,
                  debug=self.debug,
                  debugfile=self.debugfile,
                  tabmodule=self.tabmodule)

        if self.file_name==None:
           while 1:
                try:
                    s = raw_input('datcom > ')
                except EOFError:
                    break
                if not s: continue     
                yacc.parse(s)
        else:
            with open(self.file_name) as f:
                file_data = f.read();
            yacc.parse(file_data)
     
class DatcomParser(Parser):

    re_float = r'(\+|-)?\d+\.\d+(E(\+|-)?\d+)?'
    re_float_vector = r'({f})(\s*,\s*{f})*'.format(f=re_float)

    states = (
        ('CASE','exclusive'),
        ('INPUT','exclusive'),
        ('STATIC','exclusive'),
        ('DYNAMIC','exclusive'),
        ('SYMFLAP','exclusive'),
        ('ASYFLAP','exclusive'),
    )

    reserved_INPUT = {
        'NACA' : 'NACA',
        'DIM'  : 'DIM',
        'DAMP'  : 'DAMP',
        'PART'  : 'PART',
        'DERIV' : 'DERIV',
    }

    reserved_NAMELISTS = [
        'FLTCON',
        'SYNTHS',
        'BODY',
        'WGPLNF',
        'SYMFLP',
        'ASYFLP',
        'OPTINS',
        'HTPLNF',
        'VTPLNF',
        'VFPLNF',
        'WGSCHR',
        'HTSCHR',
        'VTSCHR',
        'VFSCHR',
        'PROPWR',
        'JETPWR']

    tokens = [
        'NAME',
        'LPAREN',
        'RPAREN',
        'NEWCASE',
        'DYNAMICTABLE',
        'STATICTABLE',
        'ASYFLAPTABLE',
        'SYMFLAPTABLE',
        'FLOATVECTOR',
        'INTEGER',
        'EQUALS',
        'ENDNAMELIST', # delimeter
        'COMMA',
        'BOOL',
        'CASEID',
        'NAMELIST'] \
        + reserved_INPUT.values()

    # Tokens

    t_ANY_LPAREN = '\('
    t_ANY_RPAREN = '\)'

    t_INITIAL_ignore = ' \t' 

    t_ANY_ignore = ' \t'

    t_INPUT_EQUALS  = r'='

    t_INPUT_ignore = ''

    def t_INPUT_BOOL(self, t):
        r'(\.TRUE\.|\.FALSE\.)'
        return t

    def t_INPUT_NEXTCASE(self, t):
        r'NEXT\ CASE'

    def t_CASE_NEWCASE(self, t):
        r'.*THE\ FOLLOWING\ IS\ A\ LIST\ OF\ ALL.*'
        # pop old case
        # starts within input first
        t.lexer.push_state('INPUT')
        #print 'new case'
        return t

    def t_INITIAL_NEWCASE(self, t):
        r'.*THE\ FOLLOWING\ IS\ A\ LIST\ OF\ ALL.*'
        t.lexer.push_state('CASE')
        t.lexer.push_state('INPUT')
        #print 'new case'
        return t

    def t_CASE_end_CASE(self, t):
        r'1\ END\ OF\ JOB\.'
        t.lexer.pop_state()

    def t_CASE_begin_DYNAMIC(self, t):
        r'DYNAMIC\ DERIVATIVES\ \(PER.*\n(0.*\n){3}'
        #print 'begin dynamic'
        t.lexer.push_state('DYNAMIC')

    def t_CASE_begin_STATIC(self, t):
        r'.*DERIVATIVE\ \(PER.*\n(0.*\n){2}'
        #print 'begin static'
        t.lexer.push_state('STATIC')

    def t_CASE_begin_SYMFLAP(self, t):
        r'CHARACTERISTICS\ OF\ HIGH\ LIFT\ AND\ CONTROL.*(.*\n){8}.*INCREMENTS\ DUE\ TO\ DEFLECTION.*'
        print 'begin symflap'
        t.lexer.push_state('SYMFLAP')

    def t_CASE_begin_ASYFLAP(self, t):
        r'CHARACTERISTICS\ OF\ HIGH\ LIFT\ AND\ CONTROL.*(.*\n){8}.*YAWING\ MOMENT\ COEFFICIENT.*'
        print 'begin asyflap'
        t.lexer.push_state('ASYFLAP')

    def t_ASYFLAP_SYMFLAP_STATIC_DYNAMIC_end(self, t):
        r'0'
        #print 'end'
        t.lexer.pop_state()

    def t_ASYFLAP_SYMFLAP_STATIC_DYNAMIC_table(self, t):
        r'([^0].+\n?)+'
        state = t.lexer.lexstate
        t.type = state+'TABLE'
        return t

    def t_INPUT_COMMA(self, t):
        r','
        return t

    def t_INPUT_NAMELIST(self, t):
        '\$(?P<name>[A-Z]+)'
        t.value = t.lexer.lexmatch.group('name')
        if t.value in self.reserved_NAMELISTS:
            return t
        else:
            self.t_ANY_error(t)

    def t_INPUT_ENDNAMELIST(self, t):
        '\$'
        #print 'delim'
        return t

    def t_ANY_newline(self, t):
        r'\n'
        t.lexer.lineno += t.value.count("\n")
        #print 'newline'

    def t_ANY_error(self, t):
        print("Illegal character '%s' at line" % t.value[0], t.lexer.lineno)
        t.lexer.skip(1)
        sys.exit(1)

    def t_INPUT_end_INPUT(self, t):
        r'1.*AUTOMATED\ STABILITY\ AND\ CONTROL\ METHODS.*'
        #print 'end input'
        t.lexer.pop_state()

    def t_INPUT_CASEID(self, t):
        r'.*CASEID (?P<name>.*)'
        t.value = t.lexer.lexmatch.group('name').strip()
        t.num = len(self.cases)
        #print t.value
        return t

    def t_INPUT_NAME(self, t):
        r'[a-zA-Z_][a-zA-Z0-9_]*'
        t.type = self.reserved_INPUT.get(t.value,'NAME')
        #print t
        return t

    def t_CASE_INITIAL_JUNKALL(self, t):
        r'.+'

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
        #print t
        return t

    def t_INPUT_ZEROLINE(self, t):
        r'0\s.*'
        #print 'zeroline'

    def t_ANY_INTEGER(self, t):
        r'\d+'
        try:
            t.value = int(t.value)
        except ValueError:
            print("Could not create int from %s" % t.value)
            t.value = 0
        #print t
        return t

    # Parsing rules

    precedence = ()

    # first rule is top-level rule
    def p_file_statement_append(self, p):
        """
        file : statement file
        """
        if p[2] == None:
            p[2] = [ p[1] ]
        else:
            p[0] = p[2].append(p[1])

    def p_file_from_statement(self, p):
        """
        file : statement
        """
        p[0] = [ p[1] ]

    def p_newcase(self, p):
        """
        statement : NEWCASE
        """
        self.cases.append({})
        #print '\n'

    def p_caseid(self, p):
        """
        statement : CASEID
        """
        self.cases[-1]['ID'] = p[1]

    def parse_table1d(self,cols,data):
        table = {}
        colLastOld = -1
        for i in xrange(len(cols)):
            colLast = colLastOld+cols[i][1]
            lines = data.split('\n')
            valList = []
            for j in xrange(len(lines)-1): # TODO why -1
                line = lines[j]
                col = line[colLastOld+1:colLast].strip()
                if col == '':
                    val = 0
                elif col == 'NDM':
                    val = 'NDM'                    
                elif col == 'NA':
                    val = 'NA'
                else:
                    #print 'raw: %11s' % col
                    val = float(col)
                    #print 'float: %11f\n' % val
                valList.append(val)
            table[cols[i][0]] = valList
            colLastOld = colLast
        return  table

    def p_dynamictable(self, p):
        """
        statement : DYNAMICTABLE
        """
        self.cases[-1]['DYNAMIC'] = self.parse_table1d(
            [['ALPHA', 10], ['CLQ', 13], ['CMQ', 13],
            ['CLAD', 13], ['CMAD', 13], ['CLP', 14],
            ['CYP', 13], ['CNP', 13], ['CNR', 13],
            ['CLR', 14]], p[1])

    def p_statictable(self, p):
        """
        statement : STATICTABLE
        """
        self.cases[-1]['STATIC'] = self.parse_table1d(
            [['ALPHA', 8], ['CD', 9], ['CL', 9],
            ['CM', 10], ['CN', 8], ['CA', 9],
            ['XCP', 9], ['CLA', 13], ['CMA', 13],
            ['CYB', 13], ['CNB', 14], ['CLB', 13]], p[1])

    def p_asyflaptable(self, p):
        """
        statement : ASYFLAPTABLE
        """
        print p[1]

    def p_symflaptable(self, p):
        """
        statement : SYMFLAPTABLE
        """
        print p[1]

    #def parse_table2d(self,cols,data):
        #table = {}
        #colLastOld = -1
        #for i in xrange(len(cols)):
            #colLast = colLastOld+cols[i][1]
            #lines = data.split('\n')
            #valList = []
            #for j in xrange(len(lines)-1): # TODO why -1
                #line = lines[j]
                #col = line[colLastOld+1:colLast].strip()
                #if col == '':
                    #val = 0
                #elif col == 'NDM':
                    #val = 'NDM'                    
                #elif col == 'NA':
                    #val = 'NA'
                #else:
                    ##print 'raw: %11s' % col
                    #val = float(col)
                    ##print 'float: %11f\n' % val
                #valList.append(val)
            #table[cols[i][0]] = valList
            #colLastOld = colLast
        #return  table

    def p_error(self, p):
        if p:
            print("Syntax error '%s' at line: %d" % 
                  (p.value, self.lex.lineno))
        else:
            print("Syntax error at EOF")
        sys.exit(1)

    def p_namelist(self, p):
        """
        statement : NAMELIST terms ENDNAMELIST
        """
        self.cases[-1][p[1]] = p[2]

    def p_float_term(self, p):
        """
        term : NAME EQUALS FLOATVECTOR
        """
        p[0] = {p[1] : p[3]}
        #print 'term'

    def p_bool_term(self, p):
        """
        term : NAME EQUALS BOOL
        """
        if p[3] == ".TRUE.":
            p[0] = { p[1] : True }
        elif p[3] == ".FALSE.":
            p[0] = { p[1] : False }
        else:
            self.p_error(p[3])

    def p_airfoil(self, p):
        """
        statement : NACA NAME INTEGER INTEGER
        """
        self.cases[-1][p[2]] = p[4]

    def p_dim(self, p):
        """
        statement : DIM NAME
        """
        self.cases[-1][p[1]] = p[2]

    def p_damp(self, p):
        """
        statement : DAMP
        """
        self.cases[-1][p[1]] = True

    def p_part(self, p):
        """
        statement : PART
        """
        self.cases[-1][p[1]] = True

    def p_deriv(self, p):
        """
        statement : DERIV NAME
        """
        self.cases[-1][p[1]] = p[2]

    def p_term_terms(self, p):
        """
        terms : term
        """
        p[0] = p[1]

    def p_terms(self, p):
        """
        terms : terms COMMA term
        """
        p[0] = p[1]
        for key in p[3].keys():
            if key in p[0]:
                raise KeyError('duplicate key %s' % key)
            else:
                p[0][key] = p[3][key]

    def p_array_term(self, p):
        """
        term : NAME LPAREN INTEGER RPAREN EQUALS FLOATVECTOR 
        """
        p[0] = {p[1] : p[6]}

if __name__ == '__main__':
    if len(sys.argv) == 1:
        parser = DatcomParser()
    else:
        parser = DatcomParser({
            'file_name':sys.argv[1]
            })
    print 'cases:'
    for case in parser.cases:
        print '\n', case.get('ID','UNKNOWN'), '\n', case.keys()
        if 'STATIC' in case.keys():
            print 'CL:', case['STATIC']['CL']
