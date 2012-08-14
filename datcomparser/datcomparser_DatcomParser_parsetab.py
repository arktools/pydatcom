
# datcomparser_DatcomParser_parsetab.py
# This file is automatically generated. Do not edit.
_tabversion = '3.2'

_lr_method = 'LALR'

_lr_signature = '\x82\x04\xbf\xaa\xd5\xb3iayz\x9f*\xc1K\xeci'
    
_lr_action_items = {'NAMELIST':([0,1,4,6,7,8,10,11,12,14,19,21,24,27,],[5,-3,-5,-7,-14,-4,-15,-6,5,-8,-13,-16,-9,-12,]),'DIM':([0,1,4,6,7,8,10,11,12,14,19,21,24,27,],[9,-3,-5,-7,-14,-4,-15,-6,9,-8,-13,-16,-9,-12,]),'FLOATVECTOR':([25,33,],[29,34,]),'PART':([0,1,4,6,7,8,10,11,12,14,19,21,24,27,],[10,-3,-5,-7,-14,-4,-15,-6,10,-8,-13,-16,-9,-12,]),'ASYFLPTABLE':([0,1,4,6,7,8,10,11,12,14,19,21,24,27,],[14,-3,-5,-7,-14,-4,-15,-6,14,-8,-13,-16,-9,-12,]),'RPAREN':([31,],[32,]),'DYNAMICTABLE':([0,1,4,6,7,8,10,11,12,14,19,21,24,27,],[4,-3,-5,-7,-14,-4,-15,-6,4,-8,-13,-16,-9,-12,]),'DAMP':([0,1,4,6,7,8,10,11,12,14,19,21,24,27,],[7,-3,-5,-7,-14,-4,-15,-6,7,-8,-13,-16,-9,-12,]),'COMMA':([16,17,28,29,30,34,],[-17,23,-18,-10,-11,-19,]),'INTEGER':([15,22,26,],[22,27,31,]),'$end':([1,2,4,6,7,8,10,11,12,14,19,20,21,24,27,],[-3,0,-5,-7,-14,-4,-15,-6,-2,-8,-13,-1,-16,-9,-12,]),'NACA':([0,1,4,6,7,8,10,11,12,14,19,21,24,27,],[3,-3,-5,-7,-14,-4,-15,-6,3,-8,-13,-16,-9,-12,]),'ENDNAMELIST':([16,17,28,29,30,34,],[-17,24,-18,-10,-11,-19,]),'EQUALS':([18,32,],[25,33,]),'LPAREN':([18,],[26,]),'DERIV':([0,1,4,6,7,8,10,11,12,14,19,21,24,27,],[13,-3,-5,-7,-14,-4,-15,-6,13,-8,-13,-16,-9,-12,]),'NEWCASE':([0,1,4,6,7,8,10,11,12,14,19,21,24,27,],[1,-3,-5,-7,-14,-4,-15,-6,1,-8,-13,-16,-9,-12,]),'NAME':([3,5,9,13,23,],[15,18,19,21,18,]),'SYMFLPTABLE':([0,1,4,6,7,8,10,11,12,14,19,21,24,27,],[6,-3,-5,-7,-14,-4,-15,-6,6,-8,-13,-16,-9,-12,]),'CASEID':([0,1,4,6,7,8,10,11,12,14,19,21,24,27,],[8,-3,-5,-7,-14,-4,-15,-6,8,-8,-13,-16,-9,-12,]),'BOOL':([25,],[30,]),'STATICTABLE':([0,1,4,6,7,8,10,11,12,14,19,21,24,27,],[11,-3,-5,-7,-14,-4,-15,-6,11,-8,-13,-16,-9,-12,]),}

_lr_action = { }
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = { }
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'term':([5,23,],[16,28,]),'terms':([5,],[17,]),'statement':([0,12,],[12,12,]),'file':([0,12,],[2,20,]),}

_lr_goto = { }
for _k, _v in _lr_goto_items.items():
   for _x,_y in zip(_v[0],_v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = { }
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> file","S'",1,None,None,None),
  ('file -> statement file','file',2,'p_file_statement_append','./datcomparser.py',261),
  ('file -> statement','file',1,'p_file_from_statement','./datcomparser.py',270),
  ('statement -> NEWCASE','statement',1,'p_newcase','./datcomparser.py',276),
  ('statement -> CASEID','statement',1,'p_caseid','./datcomparser.py',283),
  ('statement -> DYNAMICTABLE','statement',1,'p_dynamictable','./datcomparser.py',314),
  ('statement -> STATICTABLE','statement',1,'p_statictable','./datcomparser.py',325),
  ('statement -> SYMFLPTABLE','statement',1,'p_syMflptable','./datcomparser.py',336),
  ('statement -> ASYFLPTABLE','statement',1,'p_asymflptable','./datcomparser.py',347),
  ('statement -> NAMELIST terms ENDNAMELIST','statement',3,'p_namelist','./datcomparser.py',391),
  ('term -> NAME EQUALS FLOATVECTOR','term',3,'p_float_term','./datcomparser.py',397),
  ('term -> NAME EQUALS BOOL','term',3,'p_bool_term','./datcomparser.py',404),
  ('statement -> NACA NAME INTEGER INTEGER','statement',4,'p_airfoil','./datcomparser.py',415),
  ('statement -> DIM NAME','statement',2,'p_dim','./datcomparser.py',421),
  ('statement -> DAMP','statement',1,'p_damp','./datcomparser.py',427),
  ('statement -> PART','statement',1,'p_part','./datcomparser.py',433),
  ('statement -> DERIV NAME','statement',2,'p_deriv','./datcomparser.py',439),
  ('terms -> term','terms',1,'p_term_terms','./datcomparser.py',445),
  ('terms -> terms COMMA term','terms',3,'p_terms','./datcomparser.py',451),
  ('term -> NAME LPAREN INTEGER RPAREN EQUALS FLOATVECTOR','term',6,'p_array_term','./datcomparser.py',462),
]