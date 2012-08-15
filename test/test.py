#!/usr/bin/env python

import unittest
import os
from datcomparser import DatcomParser, DatcomExporter

class Test(unittest.TestCase):

    def setUp(self):
        pass

    def test1(self):
        parser = DatcomParser(os.path.join('test','data','Citation.out'))
        for case in parser.get_cases():
            print 'case: %s\n%s\n' % (case['ID'],case.keys())
        exporter =  DatcomExporter(parser.get_cases(), 'modelica.mo')
        print exporter.get_export()
