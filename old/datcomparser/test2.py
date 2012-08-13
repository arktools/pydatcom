#!/usr/bin/env python
import re
import sys

re_input_header = re.compile('.*THE FOLLOWING IS A LIST OF ALL INPUT CARDS FOR THIS CASE\..*')
re_caseid = re.compile('.*CASEID (.*)')
re_input_group = re.compile('$FLTCON(.*)$',re.M)

with open(sys.argv[1]) as f:
  lines=f.readlines()
for line in lines:
  match = re_caseid.match(line)
  if match != None:
    print match.group(1)
  match = re_input_header.match(line)
  if match != None:
    print match.group(1)

# vim:ts=2:sw=2:expandtab
