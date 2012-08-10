#!/usr/bin/env python
from jinja2 import Environment, PackageLoader
env = Environment(loader=PackageLoader('datcomparser', 'templates'))


my_dict = {
  'name': 'test',
  'CL_Basic' : [4,5],
  'dCL_Flap' : [[0,1],[1,2]],
  'alpha' : [0,1],
  'flap' : [0,1],
}

template = env.get_template('modelica.mo')

print template.render(my_dict)

# vim:ts=2:sw=2:expandtab
