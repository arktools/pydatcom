#!/usr/bin/env python
import os
import re
from jinja2 import Environment, PackageLoader

from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
from matplotlib import cm
from matplotlib.ticker import EngFormatter
import numpy as np


class DatcomPlotter(object):

    def __init__(self, parser_dict):
        self.d = parser_dict
        if not os.path.isdir('fig'):
            os.mkdir('fig')
        self.figpath = os.path.abspath('fig')

    def common_plots(self):
        ## lift plots
        self.plot2d(
            title='{name}: Basic Lift Coefficient',
            x_name='alpha', x_label='Alpha, deg',
            y_name='CL_Basic', y_label='CL')
        self.plot2d(
            title='{name}: Flap effect on Lift Coefficient',
            x_name='flap', x_label='Flap, deg',
            y_name='dCL_Flap', y_label='dCL')
        self.plot2d(
            title='{name}: Elevator effect on Lift Coefficient',
            x_name='elev', x_label='Elevator, deg',
            y_name='dCL_Elevator', y_label='dCL')
        self.plot2d(
            title='{name}: Pitch Rate effect on Lift Coefficient',
            x_name='alpha', x_label='Alpha, deg',
            y_name='dCL_PitchRate', y_label='dCL')
        self.plot2d(
            title='{name}: Alpha Dot effect on Lift Coefficient',
            x_name='alpha', x_label='Alpha, deg',
            y_name='dCL_AlphaDot', y_label='dCL')

        ## drag plots
        self.plot2d(
            title='{name}: Basic Drag Coefficient',
            x_name='alpha', x_label='Alpha, deg',
            y_name='CD_Basic', y_label='CD')
        self.plot2d(
            title='{name}: Drag Polar',
            x_name='CL_Basic', x_label='CL',
            y_name='CD_Basic', y_label='CD')
        self.plot3d(
            title='{name}: Flap effect on Drag Coefficient',
            x_name='alpha', x_label='Alpha, deg',
            y_name='flap', y_label='Flap, deg',
            z_name='dCD_Flap', z_label='dCD')
        self.plot3d(
            title='{name}: Elevator effect on Drag Coefficient',
            x_name='alpha', x_label='Alpha, deg',
            y_name='elev', y_label='Elevator, deg',
            z_name='dCD_Elevator', z_label='dCD')

        ## side force plots
        self.plot2d(
            title='{name}: Basic Side Force Coefficient',
            x_name='alpha', x_label='Alpha, deg',
            y_name='dCY_Beta', y_label='dCY')
        self.plot2d(
            title='{name}: Roll Rate effect on Side Force Coefficient',
            x_name='alpha', x_label='Alpha, deg',
            y_name='dCY_RollRate', y_label='dCY')

        ## roll moment
        self.plot2d(
            title='{name}: Aileron effect on Roll Moment Coefficient',
            x_name='alrn', x_label='Aileron, deg',
            y_name='dCl_Aileron', y_label='dCl')
        self.plot2d(
            title='{name}: Side slip effect on Roll Moment Coefficient',
            x_name='alpha', x_label='Alpha, deg',
            y_name='dCl_Beta', y_label='dCl')
        self.plot2d(
            title='{name}: RollRate effect on Roll Moment Coefficient',
            x_name='alpha', x_label='Alpha, deg',
            y_name='dCl_RollRate', y_label='dCl')
        self.plot2d(
            title='{name}: YawRate effect on Roll Moment Coefficient',
            x_name='alpha', x_label='Alpha, deg',
            y_name='dCl_YawRate', y_label='dCl')

        ## pitch moment
        self.plot2d(
            title='{name}: Basic Pitch Moment Coefficient',
            x_name='alpha', x_label='Alpha, deg',
            y_name='Cm_Basic', y_label='Cm')
        self.plot2d(
            title='{name}: Flap effect on Pitch Moment Coefficient',
            x_name='flap', x_label='Flap, deg',
            y_name='dCm_Flap', y_label='dCm')
        self.plot2d(
            title='{name}: Elevator effect on Pitch Moment Coefficient',
            x_name='elev', x_label='Elevator, deg',
            y_name='dCm_Elevator', y_label='dCm')
        self.plot2d(
            title='{name}: Pitch Rate effect on Pitch Moment Coefficient',
            x_name='alpha', x_label='Alpha, deg',
            y_name='dCm_PitchRate', y_label='dCm')
        self.plot2d(
            title='{name}: Alpha Dot effect on Pitch Moment Coefficient',
            x_name='alpha', x_label='Alpha, deg',
            y_name='dCm_AlphaDot', y_label='dCm')

        ## yaw moment
        self.plot3d(
            title='{name}: Aileron effect on Yaw Moment Coefficient',
            x_name='alpha', x_label='Alpha, deg',
            y_name='flap', y_label='Flap, deg',
            z_name='dCn_Aileron', z_label='dCn')
        self.plot2d(
            title='{name}: Side Slip effect on Yaw Moment Coefficient',
            x_name='alpha', x_label='Alpha, deg',
            y_name='dCn_Beta', y_label='dCn')
        self.plot2d(
            title='{name}: Roll Rate effect on Yaw Moment Coefficient',
            x_name='alpha', x_label='Alpha, deg',
            y_name='dCn_RollRate', y_label='dCn')
        self.plot2d(
            title='{name}: Yaw Rate effect on Yaw Moment Coefficient',
            x_name='alpha', x_label='Alpha, deg',
            y_name='dCn_YawRate', y_label='dCn')

    def plot2d(self, title,
               x_name, x_label,
               y_name, y_label):
        fig = plt.figure()
        ax = fig.add_subplot(111)
        y = self.d[y_name]
        x = self.d[x_name][:len(y)]
        ax.plot(x, y)
        ax.set_xlabel(x_label.format(**self.d))
        ax.set_ylabel(y_label.format(**self.d))
        ax.set_title(title.format(**self.d))
        ax.grid()
        plt.savefig(os.path.join(self.figpath,
            os.path.join(self.figpath,
                title.format(**self.d) + '.pdf')))
        plt.close(fig)

    def plot3d(self, title,
               x_name, x_label,
               y_name, y_label,
               z_name, z_label):
        fig = plt.figure()
        ax = fig.add_subplot(111, projection='3d')
        Z = np.transpose(self.d[z_name])
        x = self.d[x_name][:len(Z[0])]
        y = self.d[y_name][:len(Z)]
        ax.set_xlabel(x_label.format(**self.d))
        ax.set_ylabel(y_label.format(**self.d))
        ax.set_zlabel(z_label.format(**self.d))
        ax.set_title(title.format(**self.d))
        #print 'len Z1:', len(Z)
        #print 'len Z2:', len(Z[0])
        #print 'len x:', len(x)
        #print 'len y:', len(y)
        X, Y = np.meshgrid(x, y)
        surf = ax.plot_surface(X, Y, Z,
            cmap=cm.jet, rstride=1, cstride=1)
        fig.colorbar(surf, shrink=0.5, aspect=5)
        ax.grid()
        plt.savefig(os.path.join(self.figpath,
            os.path.join(self.figpath,
                title.format(**self.d) + '.pdf')))
        plt.close(fig)

    @staticmethod
    def command_line():
        import argparse
        from parser import DatcomParser

        argparser = argparse.ArgumentParser()
        argparser.add_argument("datcom_file",
            help="the output file from datcom to parse")
        args = argparser.parse_args()

        parser = DatcomParser(args.datcom_file)
        plotter = DatcomPlotter(parser.get_common())
        plotter.common_plots()

if __name__ == "__main__":
    DatcomPlotter.command_line()
