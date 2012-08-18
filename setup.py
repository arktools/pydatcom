#!/usr/bin/env python
from setuptools import setup, find_packages
import sys
import os

version = '0.2.5'

setup(name='PyDatcom',
      version=version,
      description="A python interface for DATCOM. Provides export/ parsing capabilities.",
      long_description="""\
      Parses DATCOM using ply and exports
      using Jinja2 for flexibility. Custom
      Jinja2 templates can be utilized to
      add more output formats.
      """,
      classifiers=[
          'Development Status :: 4 - Beta',
          'Environment :: Console',
          'Intended Audience :: Science/Research',
          'License :: OSI Approved :: GNU General Public License v3 (GPLv3)',
          'Operating System :: OS Independent',
          'Programming Language :: Python :: 2.7',
          'Topic :: Scientific/Engineering',
          'Topic :: Text Processing :: General',
      ],
      # Get strings from
      # http://pypi.python.org/pypi?%3Aaction=list_classifiers
      keywords='',
      author='James Goppert',
      author_email='james.goppert@gmail.com',
      url='https://github.com/arktools/pydatcom',
      license='GPLv3',
      packages=find_packages(exclude=['ez_setup', 'examples', 'tests']),
      include_package_data=True,
      install_requires=['jinja2', 'ply', 'matplotlib'],
      package_dir={'pydatcom': 'pydatcom'},
      package_data={'pydatcom': ['templates/*']},
      entry_points={
        'console_scripts': [
            'pydatcom-export = pydatcom:DatcomExporter.command_line',
            'pydatcom-plot = pydatcom:DatcomPlotter.command_line'
        ]},
      )
