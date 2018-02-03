#!/usr/bin/python3

import glob
import os
import re

import tika
from tika import parser


# start the tika VM
tika.initVM()



# move to where documents are
os.chdir( 'data')

# open file to write
with open('terms.output', 'a+') as out:
    files = glob.glob('*.pdf')
    files.sort()

    # there is something wrong with the first two files.
    for f in files[2:]:
        try:
            # get the data from the server
            parsed = parser.from_file( f )

            # files are broken into metadata, content
            # filter for usable terms
            terms = re.findall('\w+', parsed['content'])

            # write them to the file
            for term in terms:
                out.write( term + ',\n')
        except:
            print( "The file " + f + " encountered an error")


# 67 to skip into stuff
# -26 to cut bottom stuff
