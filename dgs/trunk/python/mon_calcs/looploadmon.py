#!/usr/bin/python

import moncalc as mc
import pylab as pl

# loop over a list of monitor files, and save the output as a big text file
# filenamestring is the filename to save the data to (string)
def looploadmon(filenamestring):

    path = "/SNS/users/5us/data/SNS/ARCS/2008_3_18_SCI/15"

    #filenumbers 919 through 988 (add one onto end in range function)
    filenums = range(919,989)
    freqar   = [600, 600, 600, 600, 600, 600, 600, \
                540, 540, 540, 540, 540, 540, 540, \
                480, 480, 480, 480, 480, 480, 480, \
                420, 420, 420, 420, 420, 420, 420, \
                360, 360, 360, 360, 360, 360, 360, \
                300, 300, 300, 300, 300, 300, 300, \
                240, 240, 240, 240, 240, 240, 240, \
                180, 180, 180, 180, 180, 180, 180, \
                120, 120, 120, 120, 120, 120, 120, \
                60 , 60 , 60 , 60 , 60 , 60 , 60]
                
    energyar = [700, 600, 500, 400, 300, 200, 100, \
                700, 600, 500, 400, 300, 200, 100, \
                700, 600, 500, 400, 300, 200, 100, \
                700, 600, 500, 400, 300, 200, 100, \
                700, 600, 500, 400, 300, 200, 100, \
                700, 600, 500, 400, 300, 200, 100, \
                700, 600, 500, 400, 300, 200, 100, \
                700, 600, 500, 400, 300, 200, 100, \
                700, 600, 500, 400, 300, 200, 100, \
                700, 600, 500, 400, 300, 200, 100 ]

    counter = 0

    outarray = []

    for x in filenums:
        aout = mc.moncalc(path,'ARCS',x,11.825,18.5,energyar[counter],freqar[counter])
        counter = counter + 1
        #append the current aout list to the large output array
        outarray.append(aout)

    
    
    pl.save(filenamestring, outarray, fmt="%.4e")









    



    
