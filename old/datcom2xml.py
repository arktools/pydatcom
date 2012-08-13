#!/usr/bin/env python

# Lenna X. Peterson
# arklenna@gmail.com

import sys
import os
import re
from math import floor, ceil

class Datcom2xml:
    ##### Constructor #####
    def __init__(self, infile, outfile):
        #### Constants ####
        self._title_str = "-DERIVATIVE (PER DEGREE)-"
        self._header_re = re.compile(r"0\s+ALPHA")
        self._footer_re = re.compile(r"1\s+")
        
        #### Files ####
        if not os.access(infile, os.R_OK):
            sys.stderr.write("Could not access datcom file '%s'\n" % infile)
            raise SystemExit
        self._datcom = infile
        self._xml = outfile
        
        #### Containers ####
        self._headers = []
        self._rows = []
        self._data = dict()
        
        #### Call methods ####
        self._readDatcom()
        self._parseDatcom()
        #self._writeDatcom()
        
    
    
    ##### Methods #####
    def _readDatcom(self):
        """Collect header and data lines from the table"""
        with open(self._datcom) as fh:
            print "Opened file"
            ### Set flags
            title = 0
            header = 0
            for line in fh:
                if title:
                    ### Stop looping at footer
                    if header and re.match(self._footer_re, line):
                        print "Found footer:", line
                        break
                    ### Find, count, and store all header rows
                    elif re.match(self._header_re, line):
                        header += 1
                        self._headers.append((header,line))
                        print "Found header:", line
                    ### Store all data rows
                    elif header:
                        if line.strip() == "0":
                            continue
                        self._rows.append((header,line))
                        #print "Found row: %s (%s)" % (line, header) 
                else:
                    ### Start storing at title
                    if self._title_str in line:
                        title += 1
                        print "Found title:", line
    
    def _parseDatcom(self):
        ### Loop through headers to detect whitespace to slice data lines
        breakpoints = dict()
        col_names = dict()
        for head_num, line in self._headers:
            if len(self._headers) == 0:
                break
            # replace leading 0 with space
            line = re.sub(r"^0", " ", line)
            # find all pairs of (whitespace, non-whitespace)
            i = re.finditer(r"(\s*)([^\s]+)", line)
            space_len = []
            columns = []
            col_len = []
            for m in i:
                space = m.group(1)
                space_len.append(len(space))
                col = m.group(2)
                columns.append(col)
                col_len.append(len(col))

            # first value is 0 for [0:x] slice
            header_breakpoints = [0]
            base = 0

            for n in xrange(1, len(columns)):
                # prev col width + smaller half of trailing whitespace
                right_half = col_len[n-1] + int(floor(0.5*space_len[n]))
                if n == 1:
                    # all of 0th whitespace
                    left_half = space_len[n-1]
                else:
                    # larger half of leading whitespace
                    left_half = int(ceil(0.5*space_len[n-1]))
                # half leading whitespace, col width, half trailing whitespace
                width = left_half + right_half
                point = base + width
                # set base to current breakpoint
                base = point
                header_breakpoints.append(point)

            # store header cols and breakpoints keyed by head_num
            col_names[head_num] = columns
            breakpoints[head_num] = header_breakpoints

        #print breakpoints

        ### Initialize data storage
        for v in col_names.itervalues():
            for n in v:
                self._data[n] = []
        ### Slice data lines based on header whitespace
        for head_num, line in self._rows:
            bp = breakpoints[head_num]
            cols = col_names[head_num]
            num_cols = len(bp)
            for k in xrange(num_cols):
                # only store alpha once
                if head_num > 1 and cols[k] == "ALPHA":
                    continue
                # last value
                if k == num_cols-1:
                    cell = line[bp[k]:]
                else:
                    cell = line[bp[k]:bp[k+1]]
                self._data[cols[k]].append(cell.strip())
                #print cell.strip()
            #print "\n"
        print self._data
        alpha_w = self._maxLen(self._data['ALPHA']) + 2
        cla_w = self._maxLen(self._data['CLA']) + 2
        for n in xrange(len(self._data['CLA'])):
            pair = (self._data['ALPHA'][n], self._data['CLA'][n])
            # Build format string using computed max widths
            string = "%" + str(alpha_w) + "s %" + str(cla_w) + "s"
            #print string % pair
        
    
    def _maxLen(self, _list):
        """Find longest len() in list (for automatic string formatting)"""
        maxLen = 0
        for cell in _list:
            if len(cell) > maxLen:
                maxLen = len(cell)
        return maxLen

    def _writeDatcom(self):
        with open(outfile, "w") as fh:
            pass

if __name__ == "__main__":

    if len(sys.argv) == 3:
        infile = sys.argv[1]
        outfile = sys.argv[2]
        d = Datcom2xml(infile, outfile)
    else:
        sys.stderr.write("Args: input_file output_file\n")
        raise SystemExit
        




# vim:sw=4:ts=4:expandtab
