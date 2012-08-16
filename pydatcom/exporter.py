#!/usr/bin/env python
import os
import re
from jinja2 import Environment, PackageLoader


class DatcomExporter(object):

    def __init__(self,
            parser_cases,
            template_file=None,
            model_name='name'):

        env = Environment(
            loader=PackageLoader('pydatcom',
                                 'templates'),
            line_statement_prefix='#')
        template = env.get_template(template_file)

        # find cases
        re_aileron = re.compile('.*aileron.*', re.I)
        re_flap = re.compile('.*flap.*', re.I)
        re_total = re.compile('.*total.*', re.I)
        cases = {}
        for case in parser_cases:
            name = case['ID']
            if re_aileron.match(name):
                cases['aileron'] = case
            elif re_flap.match(name):
                cases['flap'] = case
            elif re_total.match(name):
                cases['total'] = case
        for key in ['aileron', 'flap', 'total']:
            if key not in cases:
                raise IOError('%s case not found' % key)

        # extract some need dictionaries
        dFlap = cases['flap']['SYMFLP']
        dAileron = cases['aileron']['ASYFLP']
        dElevator = cases['total']['SYMFLP']
        dDynamic = cases['total']['DYNAMIC']
        dStatic = cases['total']['STATIC']

        # fill template dict
        template_dict = {
          'name': model_name,
          # lift
          'CL_Basic': dStatic['CL'],
          'dCL_Flap': dFlap['DERIV']['D(CL)'],
          'dCL_Elevator': dElevator['DERIV']['D(CL)'],
          'dCL_PitchRate': dDynamic['CLQ'],
          'dCL_AlphaDot': dDynamic['CLAD'],

          # drag
          'CD_Basic': dStatic['CD'],
          'dCD_Flap': dFlap['CD'],
          'dCD_Elevator': dElevator['CD'],

          # side force
          'dCY_Beta': dStatic['CYB'],
          'dCY_RollRate': dDynamic['CYP'],

          # roll moment
          'dCl_Aileron': dAileron['ROLL']['CL(ROLL)'],
          'dCl_Beta': dStatic['CLB'],
          'dCl_RollRate': dDynamic['CLP'],
          'dCl_YawRate': dDynamic['CLR'],

          # pitch moment
          'Cm_Basic': dStatic['CM'],
          'dCm_Flap': dFlap['DERIV']['D(CM)'],
          'dCm_Elevator': dElevator['DERIV']['D(CM)'],
          'dCm_PitchRate': dDynamic['CMQ'],
          'dCm_AlphaDot': dDynamic['CMAD'],

          # yaw moment
          'dCn_Aileron': dAileron['CN'],
          'dCn_Beta': dStatic['CNB'],
          'dCn_RollRate': dDynamic['CNP'],
          'dCn_YawRate': dDynamic['CNR'],

          # surfaces/ wind angles
          'flap': dFlap['DELTA'],
          'alrn': dAileron['DELTA'],
          'elev': dElevator['DELTA'],
          'alpha': dStatic['ALPHA'],
        }

        # render template
        self.export = template.render(template_dict)

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
            if args.out:
                name = os.path.splitext(args.out)[0]
            else:
                name = 'name'
            exporter = DatcomExporter(
                parser_cases=parser.get_cases(),
                template_file=args.template,
                model_name=name)
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
