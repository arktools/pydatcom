#!/usr/bin/env python

# Copyright 2012 Lenna X. Peterson (github.com/lennax)
# All rights reserved
# License: GPL v3
# This program comes with ABSOLUTELY NO WARRANTY. This is free software
# and you are welcome to distribute it under certain conditions.
# For details, see license.txt

import multiprocessing,logging
from setuptools import setup, find_packages

setup(
    name='datcomparser',
    version='0.0.1',
    license='GPL v3',
    description='Parse and converts DATCOM output files to various formats using Jinja templates',
    author='James Goppert',
    author_email='james.goppert@gmail.com',
    url='http://github.com/arktools/datcomparser',
    packages=['datcomparser'],
    package_dir={'datcomparser': 'datcomparser'},
    package_data= {'datcomparser' : ['templates/*']},
    scripts=['scripts/datcomparser.py'],
    requires=['argparse','jinja2','ply'],
    test_suite='nose.collector',
    tests_require=['nose']
)
