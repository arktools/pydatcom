#!/usr/bin/env python
from fsm import FSM, FSMError, RestartError, FSMEOF
import re
import sys
from StringIO import StringIO

class InputParser(FSM):

  re_new_case = re.compile('^1.*THE FOLLOWING IS A LIST OF ALL INPUT CARDS FOR THIS CASE..*')
  re_dim = re.compile('^ DIM +(.*)')

  def __init__(self):
    FSM.__init__(self)
    self.start('start')
    self.add('start', self.re_new_case, 'new_case', self.do_begin_input)

    #self.add('new_case', self.re_dim, 'new_case', self.do_read_dim)
    self.add('start',FSMEOF, 'done', self.cleanup)
    self.add('start', None, 'start')
    self.add('new_case', None, 'new_case')


  def do_begin_input(self, state, input):
    print self.state
    print "start of input read"

  def do_read_dim(self, state, input):
    print self.state
    print input

  def do_faq(self,state, input):
    print self.state
    print 'this is the faq'

  def do_help(self,state, input):
    print self.state
    print 'this is the help'

  def cleanup(self,state, input):
    print self.state
    print 'clean up'

if __name__ == '__main__':
  parser = InputParser()
  with open('Citation.out') as f:
    lines = f.readlines()
    lines.append(FSMEOF)
    for line in lines:
      try:
        parser.execute(line)
      except FSMError:
        sys.stderr.write('Invalid input: %r' % line)

# vim:ts=2:sw=2:expandtab;
