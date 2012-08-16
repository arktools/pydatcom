#!/usr/bin/env python

import unittest
import os
from pydatcom import DatcomParser, DatcomExporter


class Test(unittest.TestCase):

    def setUp(self):
        pass

    def test1(self):
        parser = DatcomParser(os.path.join('test', 'data', 'Citation.out'))
        for case in parser.get_cases():
            print 'case: %s\n%s\n' % (case['ID'], case.keys())
        exporter = DatcomExporter(parser.get_common(), 'modelica.mo')
        result = exporter.get_export()
        print result
