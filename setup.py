#!/usr/bin/env python
from setuptools import setup, find_packages
import sys
import os

version = '0.0.1'

setup(name='DatcomParser',
      version=version,
      description="A parser/exporter for DATCOM files.",
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
          'Operating System :: OS Independenta',
          'Programming Language :: Python :: 2.7',
          'Topic :: Scientific/Engineering',
          'Topic :: Text Processing :: General',
      ],
      # Get strings from
      # http://pypi.python.org/pypi?%3Aaction=list_classifiers
      keywords='',
      author='James Goppert',
      author_email='james.goppert@gmail.com',
      url='https://github.com/arktools/datcomparser',
      license='GPLv3',
      packages=find_packages(exclude=['ez_setup', 'examples', 'tests']),
      include_package_data=True,
      zip_safe=False,
      install_requires=['jinja2', 'ply'],
      package_dir={'datcomparser': 'datcomparser'},
      package_data={'datcomparser': ['templates/*']},
      entry_points={
        'console_scripts': [
            'datcomexport = datcomparser:DatcomExporter.command_line'
        ]},
      )
