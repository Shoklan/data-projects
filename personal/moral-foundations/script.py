#!/usr/bin/python3

import glob
import sys
import os

import tika
from tika import parser


# start the tika VM
tika.initVM()



# move to where documents are
os.chdir( '/home/ranuse/Code/data-projects/personal/moral-foundations/data')

# open file to write
with open('lines.nsv', 'a+') as out:
    for f in glob.glob('*.pdf'):
        try:
            # get the data from the server
            parsed = parser.from_file( f )

            # files are broken into metadata, content
            lines = parsed['content'].split('\n')[67:-26]

            # write them to the file
            for line in lines:
                out.write( line + '\n')
        except:
            print( "The file " + f + " encountered an error")


# 67 to skip into stuff
# -26 to cut bottom stuff
