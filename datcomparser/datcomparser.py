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

    re_float = r'(\+|-)?((\d*\.\d+)|(\d+\.))(E(\+|-)?\d+)?'
    re_float_vector = r'{f}([\n\s]*(,[\n\s]*{f})*)*'.format(f=re_float)

    states = (
        ('CASE','exclusive'),
            ('INPUT','exclusive'),
    )

    reserved_INPUT = {
        'TRIM'  : 'TRIM',
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
        'AIRFOIL',
        'NAME',
        'LPAREN',
        'RPAREN',
        'NEWCASE',
        'DYNAMICTABLE',
        'STATICTABLE',
        'ASYFLPTABLE',
        'SYMFLPTABLE',
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

    t_INPUT_ignore = ' \r\n\t'

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
        #print 'end of job'
        t.lexer.pop_state()

    def t_CASE_DYNAMICTABLE(self, t):
        r'DYNAMIC\ DERIVATIVES\n(?P<config>.*)\n(?P<case>.*)\n(.*\n){2}(?P<condition_headers>(.*\n){2})(?P<condition_units>.*)\n(?P<conditions>.*)\n(.*\n){2}0(?P<deriv_headers>.*)\n0.*\n(?P<deriv_table>([^\d\n].*\n)+)'
        match = t.lexer.lexmatch
        t.value = {
            'deriv_table' : match.group('deriv_table'),
        }
        print 'dynamic table'
        return t

    def t_CASE_STATICTABLE(self, t):
        r'CHARACTERISTICS\ AT\ ANGLE\ OF\ ATTACK.*\n(?P<config>.*(\n.*POWER.*)?)\n(?P<case>.*)\n(.*\n){2}(?P<condition_headers>(.*\n){2})(?P<condition_units>.*)\n(?P<conditions>.*)\n(.*\n){1}0(?P<deriv_headers>.*)\n0.*\n(?P<deriv_table>([^\d\n].*\n)+)'
        match = t.lexer.lexmatch
        t.value = {
            'deriv_table' : match.group('deriv_table'),
        }
        print 'static table'
        return t

    def t_CASE_SYMFLPTABLE(self, t):
        r'CHARACTERISTICS\ OF\ HIGH\ LIFT\ AND\ CONTROL\ DEVICES.*\n(?P<config>.*)\n(?P<case>.*)\n(.*\n){1}(?P<condition_headers>(.*\n){2})(?P<condition_units>.*)\n(?P<conditions>.*)\n(.*\n){1}0(?P<deriv_headers>.*)\n(.*\n){2}(?P<deriv_table>([^\d\n].*\n)+)(.*\n){2}.*INDUCED\ DRAG\ COEFFICIENT\ INCREMENT.*\n(?P<deflection>.*)\n(.*\n){2}(?P<drag_table>([^\d\n].*\n)+)'  
        match = t.lexer.lexmatch
        t.value = {
            'deriv_table' : match.group('deriv_table'),
            'deflection' : match.group('deflection'),
            'drag_table' : match.group('drag_table'),
        }
        print 'symflp table'
        return t

    def t_CASE_ASYFLPTABLE(self, t):
        r'CHARACTERISTICS\ OF\ HIGH\ LIFT\ AND\ CONTROL\ DEVICES.*\n(?P<config>.*)\n(?P<case>.*)\n(.*\n){1}(?P<condition_headers>(.*\n){2})(?P<condition_units>.*)\n(?P<conditions>.*)\n(.*\n){1}0(?P<deflection>.*)\n(.*\n){2}(?P<yaw_table>([^0].*\n)+)(.*\n){3}(?P<roll_table>([^1].*\n)+)'  
        match = t.lexer.lexmatch
        t.value = {
            'deflection' : match.group('deflection'),
            'yaw_table' : match.group('yaw_table'),
            'roll_table' : match.group('roll_table'),
        }
        print 'asyflp table'
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

    def t_INPUT_AIRFOIL(self, t):
        r'NACA[-\s]+[WHVF][-\s]+\d[-\s](?P<number>[\d-]+)?'
        #print 'airfoil'
        t.value = t.lexer.lexmatch.group('number')
        return t

    def t_INPUT_CASEID(self, t):
        r'.*CASEID (?P<name>.*)'
        t.value = t.lexer.lexmatch.group('name').strip()
        t.num = len(self.cases)
        print '\ncase:', t.value
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
            p_error(t)
            t.value = 0
        #print t
        return t

    def t_INPUT_ERROR(self, t):
        r'0.*ERROR.*\n(.+\n)'
        print 'error: %s' % t.value

    def t_INPUT_ZEROLINE(self, t):
        r'0.*'
        #print 'zeroline'

    def t_ANY_INTEGER(self, t):
        r'\d+'
        try:
            t.value = int(t.value)
        except ValueError:
            p_error(t)
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
                    try:
                        val = float(col)
                    except ValueError:
                        print 'could not cast "%s" to float' % col
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
            ['CLR', 14]],
            p[1]['deriv_table'])

    def p_statictable(self, p):
        """
        statement : STATICTABLE
        """
        self.cases[-1]['STATIC'] = self.parse_table1d(
            [['ALPHA', 8], ['CD', 9], ['CL', 9],
            ['CM', 10], ['CN', 8], ['CA', 9],
            ['XCP', 9], ['CLA', 13], ['CMA', 13],
            ['CYB', 13], ['CNB', 14], ['CLB', 13]],
            p[1]['deriv_table'])

    def p_syMflptable(self, p):
        """
        statement : SYMFLPTABLE
        """
        self.cases[-1]['CNTRL_DERIV'] = \
            p[1]['deriv_table']
        self.cases[-1]['CNTRL_DRAG'] = \
            p[1]['drag_table']
        self.cases[-1]['CNTRL_DEFLECT'] = \
            p[1]['deflection']

    def p_asymflptable(self, p):
        """
        statement : ASYFLPTABLE
        """
        self.cases[-1]['CNTRL_YAW'] = \
            p[1]['yaw_table']
        self.cases[-1]['CNTRL_ROLL'] = \
            p[1]['roll_table']
        self.cases[-1]['CNTRL_DEFLECT'] = \
            p[1]['deflection']

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
            print("Syntax error '%s' at line: %d, state: %s" % (p.value, self.lex.lineno, self.lex.lexstate))
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
        statement : AIRFOIL
        """
        self.cases[-1]['AIRFOIL'] = p[1]

    def p_trim(self, p):
        """
        statement : TRIM
        """

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
                print 'WARNING: duplicate key %s' % key
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
    print '\ncases:'
    for case in parser.cases:
        print '\n', case.get('ID','UNKNOWN'), '\n', case.keys()
        if 'STATIC' in case.keys():
            print 'CL:', case['STATIC']['CL']
