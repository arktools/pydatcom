import sys
import os
import re
import string

import ply.yacc as yacc
import ply.lex as lex
from ply.lex import TOKEN


class Parser(object):
    """
    Base class for a lexer/parser that has the rules defined as methods
    """
    tokens = ()
    precedence = ()

    def __init__(self, file_name=None, debug=0,
                keep_parse_tab=False):
        self.debug = debug
        self.file_name = file_name
        self.keep_parse_tab = keep_parse_tab
        self.cases = []
        self.common_dicts = []
        try:
            modname = os.path.split(
                os.path.splitext(__file__)[0])[1] + \
            "_" + self.__class__.__name__
        except:
            modname = "parser" + "_" + self.__class__.__name__
        self.debugfile = modname + ".dbg"
        self.tabmodule = modname + "_" + "parsetab"
        #print self.debugfile, self.tabmodule

        # Build the lexer and parser
        self.lex = lex.lex(module=self,
            debug=self.debug)
        self.yacc = yacc.yacc(module=self,
            debug=self.debug,
            debugfile=self.debugfile,
            tabmodule=self.tabmodule)

        if self.file_name == None:
            while 1:
                try:
                    s = raw_input('> ')
                except EOFError:
                    break
                if not s:
                    continue
                yacc.parse(s)
        else:
            with open(self.file_name) as f:
                file_data = f.read()
            yacc.parse(file_data)

        if not self.keep_parse_tab:
            parse_tab_file = "parser_DatcomParser_parsetab.py"
            if os.path.exists(parse_tab_file):
                os.remove(parse_tab_file)


class DatcomParser(Parser):
    """
    Parses a datcom output file.
    """

    re_float = \
        r'(\+|-)?((\d*\.\d+)|(\d+\.))(E(\+|-)?\d+)?'

    re_float_vector = \
        r'{f}([\n\s]*(,[\n\s]*{f})+)+'.format(f=re_float)

    re_dynamictable = \
        r'DYNAMIC\ DERIVATIVES\n(?P<config>.*)\n' \
        r'(?P<case>.*)\n(.*\n){2}' \
        r'(?P<condition_headers>(.*\n){2})' \
        r'(?P<condition_units>.*)\n' \
        r'(?P<conditions>.*)\n(.*\n){2}0' \
        r'(?P<deriv_headers>.*)\n0.*\n' \
        r'(?P<deriv_table>([^\d\n].*\n)+)'

    re_statictable = \
        r'CHARACTERISTICS\ AT\ ANGLE\ OF\ ATTACK.*\n' \
        r'(?P<config>.*(\n.*POWER.*)?)\n' \
        r'(?P<case>.*)\n(.*\n){2}' \
        r'(?P<condition_headers>(.*\n){2})' \
        r'(?P<condition_units>.*)\n' \
        r'(?P<conditions>.*)\n(.*\n){1}0' \
        r'(?P<deriv_headers>.*)\n0.*\n' \
        r'(?P<deriv_table>([^\d\n].*\n)+)'

    re_symflptable = \
        r'CHARACTERISTICS\ OF\ HIGH' \
        r'\ LIFT\ AND\ CONTROL\ DEVICES.*\n' \
        r'(?P<config>.*)\n(?P<case>.*)\n(.*\n){1}' \
        r'(?P<condition_headers>(.*\n){2})' \
        r'(?P<condition_units>.*)\n' \
        r'(?P<conditions>.*)\n(.*\n){1}' \
        r'0(?P<deriv_headers>.*)\n(.*\n){2}' \
        r'(?P<deriv_table>([^\d\n].*\n)+)' \
        r'(.*\n){2}.*INDUCED\ DRAG\ COEFFICIENT' \
        r'\ INCREMENT.*\n.*DELTA\ =' \
        r'(?P<deflection>.*)\n(.*\n){2}' \
        r'(?P<drag_table>([^\d\n].*\n)+)'

    re_asyflptable = \
        r'CHARACTERISTICS\ OF\ HIGH\ LIFT' \
        r'\ AND\ CONTROL\ DEVICES.*\n' \
        r'(?P<config>.*)\n(?P<case>.*)\n(.*\n){1}' \
        r'(?P<condition_headers>(.*\n){2})' \
        r'(?P<condition_units>.*)\n' \
        r'(?P<conditions>.*)\n(.*\n){1}.*' \
        r'\(DELTAL-DELTAR\)=(?P<deflection>.*)' \
        r'\n(.*\n){2}(?P<yaw_table>([^0].*\n)+)' \
        r'(.*\n){3}(?P<roll_table>([^1].*\n)+)'

    states = (
        ('CASE', 'exclusive'),
            ('INPUT', 'exclusive'),
    )

    reserved_INPUT = {
        'TRIM': 'TRIM',
        'DIM': 'DIM',
        'DAMP': 'DAMP',
        'PART': 'PART',
        'DERIV': 'DERIV',
        'DUMP': 'DUMP',
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
        'FLOAT',
        'FLOATVECTOR',
        'INTEGER',
        'EQUALS',
        'ENDNAMELIST',  # delimeter
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

    t_INPUT_EQUALS = r'='

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

    @TOKEN(re_dynamictable)
    def t_CASE_DYNAMICTABLE(self, t):
        match = t.lexer.lexmatch
        t.value = {
            'deriv_table': match.group('deriv_table'),
        }
        #print 'dynamic table'
        return t

    @TOKEN(re_statictable)
    def t_CASE_STATICTABLE(self, t):
        match = t.lexer.lexmatch
        t.value = {
            'deriv_table': match.group('deriv_table'),
        }
        #print 'static table'
        return t

    @TOKEN(re_symflptable)
    def t_CASE_SYMFLPTABLE(self, t):
        match = t.lexer.lexmatch
        t.value = {
            'deriv_table': match.group('deriv_table'),
            'deflection': match.group('deflection'),
            'drag_table': match.group('drag_table'),
        }
        #print 'symflp table'
        return t

    @TOKEN(re_asyflptable)
    def t_CASE_ASYFLPTABLE(self, t):
        match = t.lexer.lexmatch
        t.value = {
            'deflection': match.group('deflection'),
            'yaw_table': match.group('yaw_table'),
            'roll_table': match.group('roll_table'),
        }
        #print 'asyflp table'
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
        #print '\ncase:', t.value
        return t

    def t_INPUT_NAME(self, t):
        r'[a-zA-Z_][a-zA-Z0-9_]*'
        t.type = self.reserved_INPUT.get(t.value, 'NAME')
        #print t
        return t

    def t_CASE_INITIAL_JUNKALL(self, t):
        r'.+'

    @TOKEN(re_float_vector)
    def t_INPUT_FLOATVECTOR(self, t):
        try:
            vector = []
            for num in string.split(t.value, ','):
                vector.append(float(num))
            t.value = vector
        except ValueError:
            p_error(t)
            t.value = 0
        #print t
        return t

    @TOKEN(re_float)
    def t_INPUT_FLOAT(self, t):
        try:
            t.value = float(t.value)
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
            p[2] = [p[1]]
        else:
            p[0] = p[2].append(p[1])

    def p_file_from_statement(self, p):
        """
        file : statement
        """
        p[0] = [p[1]]

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

    def parse_vector(self, data):
        vector = []
        for val in data.split():
            vector.append(float(val))
        return vector

    def parse_table1d(self, cols, data):
        table = {}
        colLastOld = -1
        lines = data.split('\n')
        for i in xrange(len(cols)):
            colLast = colLastOld + cols[i][1]
            valList = []
            for j in xrange(len(lines) - 1):
            # -1 to skip last newline
                line = lines[j]
                col = line[colLastOld + 1:colLast].strip()
                if col == '' or  \
                        col == 'NDM' or \
                        col == 'NA':
                    pass
                else:
                    #print 'raw: %11s' % col
                    valList.append(float(col))
                    #print 'float: %11f\n' % val
            table[cols[i][0]] = valList
            colLastOld = colLast
        return  table

    def p_dynamictable(self, p):
        'statement : DYNAMICTABLE'
        self.cases[-1]['DYNAMIC'] = self.parse_table1d(
            [['ALPHA', 10], ['CLQ', 13], ['CMQ', 13],
            ['CLAD', 15], ['CMAD', 13], ['CLP', 14],
            ['CYP', 13], ['CNP', 13], ['CNR', 13],
            ['CLR', 14]],
            p[1]['deriv_table'])

    def p_statictable(self, p):
        'statement : STATICTABLE'
        self.cases[-1]['STATIC'] = self.parse_table1d(
            [['ALPHA', 8], ['CD', 9], ['CL', 9],
            ['CM', 10], ['CN', 8], ['CA', 9],
            ['XCP', 9], ['CLA', 13], ['CMA', 13],
            ['CYB', 13], ['CNB', 14], ['CLB', 13]],
            p[1]['deriv_table'])

    def p_symflptable(self, p):
        'statement : SYMFLPTABLE'
        data = {}
        data['DERIV'] = \
                self.parse_table1d(
            [['DELTA', 12], ['D(CL)', 10],
             ['D(CM)', 11], ['D(CL MAX)', 10],
             ['D(CD MIN)', 13], ['(CLA)D', 25],
             ['(CH)A', 12], ['(CH)D', 11]],
             p[1]['deriv_table'])
        (data['ALPHA'],
         data['CD']) = \
                self.parse_table2d(9,
            [15, 10, 10, 10, 10, 10, 10, 10, 10],
            p[1]['drag_table'])
        data['DELTA'] = \
            self.parse_vector(p[1]['deflection'])
        #print data['CNTRL_DEFLECT']
        #print self.cases[-1]['CNTRL_DRAG']
        self.cases[-1]['SYMFLP'] = data

    def p_asymflptable(self, p):
        'statement : ASYFLPTABLE'
        data = {}
        (data['ALPHA'],
         data['CN']) = \
                self.parse_table2d(7,
            [18, 12, 12, 12, 12, 12, 12, 12, 12],
            p[1]['yaw_table'])
        data['ROLL'] = \
                self.parse_table1d(
           [['DELTAL', 51], ['DELTAR', 16],
            ['CL(ROLL)', 22]],
            p[1]['roll_table'])
        data['DELTA'] = \
            self.parse_vector(p[1]['deflection'])
        self.cases[-1]['ASYFLP'] = data

    def parse_table2d(self, rowWidth, colWidths, data):
        colLastOld = -1
        dataArray = []
        rows = []
        lines = data.split('\n')

        for i in xrange(len(lines) - 1):
        # -1 to skip last newline
            line = lines[i]
            rows.append(float(line[0:rowWidth - 1]))
            colLastOld = rowWidth
            dataArray.append([])
            for j in xrange(len(colWidths)):
                colLast = colLastOld + colWidths[j]
                col = line[colLastOld + 1:colLast].strip()
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
                dataArray[-1].append(val)
                colLastOld = colLast

        return (rows, dataArray)

    def p_error(self, p):
        if p:
            print("Syntax error '%s' at line: %d, state: %s" \
                  % (p.value, self.lex.lineno, self.lex.lexstate))
        else:
            print("Syntax error at EOF")
        sys.exit(1)

    def p_namelist(self, p):
        'statement : NAMELIST terms ENDNAMELIST'
        self.cases[-1][p[1]] = p[2]

    def p_scalar_term(self, p):
        """
        term : NAME EQUALS FLOAT
        | NAME EQUALS INTEGER
        """
        p[0] = {p[1]: p[3]}
        #print 'term'

    def p_bool_term(self, p):
        'term : NAME EQUALS BOOL'
        if p[3] == ".TRUE.":
            p[0] = {p[1]: True}
        elif p[3] == ".FALSE.":
            p[0] = {p[1]: False}
        else:
            self.p_error(p[3])

    def p_airfoil(self, p):
        'statement : AIRFOIL'
        self.cases[-1]['AIRFOIL'] = p[1]

    def p_trim(self, p):
        'statement : TRIM'

    def p_dim(self, p):
        'statement : DIM NAME'
        self.cases[-1][p[1]] = p[2]

    def p_damp(self, p):
        'statement : DAMP'
        self.cases[-1][p[1]] = True

    def p_part(self, p):
        'statement : PART'
        self.cases[-1][p[1]] = True

    def p_deriv(self, p):
        'statement : DERIV NAME'
        self.cases[-1][p[1]] = p[2]

    def p_dump(self, p):
        'statement : DUMP NAME'

    def p_term_terms(self, p):
        'terms : term'
        p[0] = p[1]

    def p_terms(self, p):
        'terms : terms COMMA term'
        p[0] = p[1]
        for key in p[3].keys():
            if key in p[0]:
                print 'WARNING: duplicate key %s' % key
            else:
                p[0][key] = p[3][key]

    def p_array_term(self, p):
        """
        term : NAME LPAREN INTEGER RPAREN EQUALS FLOATVECTOR
        | NAME LPAREN INTEGER RPAREN EQUALS FLOAT
        | NAME LPAREN INTEGER RPAREN EQUALS INTEGER
        """
        p[0] = {p[1]: p[6]}

    def get_common(self):
        """
        get a dictionary of common information,
        use get_cases to get more information from
        each case
        """
        # find cases
        re_aileron = re.compile('.*aileron.*', re.I)
        re_flap = re.compile('.*flap.*', re.I)
        re_total = re.compile('.*total.*', re.I)
        cases = {}
        for case in self.cases:
            name = case['ID']
            if re_aileron.match(name):
                cases['aileron'] = case
            elif re_flap.match(name):
                cases['flap'] = case
            elif re_total.match(name):
                cases['total'] = case
        for key in ['aileron', 'flap', 'total']:
            if key not in cases:
                raise IOError('%s case not found' % key)

        # extract some need dictionaries
        dFlap = cases['flap']['SYMFLP']
        dAileron = cases['aileron']['ASYFLP']
        dElevator = cases['total']['SYMFLP']
        dDynamic = cases['total']['DYNAMIC']
        dStatic = cases['total']['STATIC']

        # create model name
        if self.file_name == None:
            model_name = 'name'
        else:
            model_name = os.path.split(os.path.splitext(self.file_name)[0])[1]

        # fill dict
        return {
          'name': model_name,
          # lift
          'CL_Basic': dStatic['CL'],
          'dCL_Flap': dFlap['DERIV']['D(CL)'],
          'dCL_Elevator': dElevator['DERIV']['D(CL)'],
          'dCL_PitchRate': dDynamic['CLQ'],
          'dCL_AlphaDot': dDynamic['CLAD'],

          # drag
          'CD_Basic': dStatic['CD'],
          'dCD_Flap': dFlap['CD'],
          'dCD_Elevator': dElevator['CD'],

          # side force
          'dCY_Beta': dStatic['CYB'],
          'dCY_RollRate': dDynamic['CYP'],

          # roll moment
          'dCl_Aileron': dAileron['ROLL']['CL(ROLL)'],
          'dCl_Beta': dStatic['CLB'],
          'dCl_RollRate': dDynamic['CLP'],
          'dCl_YawRate': dDynamic['CLR'],

          # pitch moment
          'Cm_Basic': dStatic['CM'],
          'dCm_Flap': dFlap['DERIV']['D(CM)'],
          'dCm_Elevator': dElevator['DERIV']['D(CM)'],
          'dCm_PitchRate': dDynamic['CMQ'],
          'dCm_AlphaDot': dDynamic['CMAD'],

          # yaw moment
          'dCn_Aileron': dAileron['CN'],
          'dCn_Beta': dStatic['CNB'],
          'dCn_RollRate': dDynamic['CNP'],
          'dCn_YawRate': dDynamic['CNR'],

          # surfaces/ wind angles
          'flap': dFlap['DELTA'],
          'alrn': dAileron['DELTA'],
          'elev': dElevator['DELTA'],
          'alpha': dStatic['ALPHA'],
        }

    def get_cases(self):
        return self.cases
