#!/usr/bin/env python

import unittest
import os
from pydatcom import DatcomParser, DatcomExporter, DatcomPlotter


class Test(unittest.TestCase):

    def setUp(self):
        pass

    def test_parse(self):
        parser = DatcomParser(os.path.join('test', 'data', 'Citation.out'))

    def test_export(self):
        parser = DatcomParser(os.path.join('test', 'data', 'Citation.out'))
        exporter = DatcomExporter(parser.get_common(), 'modelica.mo')
        result = exporter.get_export()

    def test_plot(self):
        parser = DatcomParser(os.path.join('test', 'data', 'Citation.out'))
        plotter = DatcomPlotter(parser.get_common())
        plotter.common_plots()
