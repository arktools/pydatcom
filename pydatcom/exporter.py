#!/usr/bin/env python
import os
import re
from jinja2 import Environment, PackageLoader


class DatcomExporter(object):

    def __init__(self,
            parser_dict,
            template_file=None):

        env = Environment(
            loader=PackageLoader('pydatcom',
                                 'templates'),
            line_statement_prefix='#')

        template = env.get_template(template_file)

        # render template
        self.export = template.render(parser_dict)

    def get_export(self):
        return self.export

    @staticmethod
    def command_line():
        import argparse
        from parser import DatcomParser

        argparser = argparse.ArgumentParser()
        argparser.add_argument("datcom_file",
            help="the output file from datcom to parse")
        argparser.add_argument("-t",
            "--template",
            help="use a jinja2 template for generation"
                 "(e.g. modelica.mo)")
        argparser.add_argument("-o",
            "--out",
            help="name of file generated from template")
        args = argparser.parse_args()

        parser = DatcomParser(args.datcom_file)
        if args.template:
            exporter = DatcomExporter(
                parser_dict=parser.get_common(),
                template_file=args.template)
            result = exporter.get_export()
            if args.out:
                with open(args.out, 'w') as f:
                    f.write(result)
            else:
                print result
        else:
            for case in parser.get_cases():
                print 'case: %s\n%s\n' % \
                        (case['ID'], case.keys())

if __name__ == "__main__":
    DatcomExporter.command_line()
