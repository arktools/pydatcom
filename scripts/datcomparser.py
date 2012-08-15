#!/usr/bin/env python
import os
import sys
import inspect

sys.path.insert(0,os.path.realpath(os.path.abspath(os.path.join(
    os.path.split(inspect.getfile( inspect.currentframe() ))[0],
    os.path.pardir))))

from datcomparser import DatcomExporter

DatcomExporter.command_line()
