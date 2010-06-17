#!/usr/bin/python

from numpy import *
from xml.dom import minidom
from scipy import *
from scipy.optimize import leastsq
from subprocess import PIPE, Popen
import csv
import string


###################################################
## comments from first review
## I need the shortlen and nbanks of CNCS and SEQ##
## Add ability to comment lines in the masktextfile.
## Check ability to have no angular range
## Think of good way to not have anything masked.
## Consider XML style formatting of masking file.
## Check spelling.
## Top is 127
## double check long and short
##
##
###################################################



"""
M. B. Stone
buliding masks for dgs reduction.
"""


###############################################
## nexus_mask_builder                        ##
###############################################
## nexus_mask_builder will build a mask based upon
## consideration of masking pixels at the end of short
## and long tubes, as well as masking between sets of
## angular ranges.
##
## assumptions: 8 tubes per pack,
##             128 pixels per tube
##
## input:
## instrument = string, 'ARCS' or 'CNCS' or 'SEQ'
## runnum = integer, some runnumber to use for an existing
##          neXus file in order to read the instrument geometry
## maskparamsfile = a text file listing the parameters for masking
##                  a complete description of this file is given below
## filename = string, the filename which will be given to the mask file.
##
##
## output:
## filename =  string, thisis the output filname
##
##
##
## The maskparamsfile
##    first row describes the number of pixels to mask at the top and bottom
##    of short and long tubes.  units are in pixels
##    [shorttubetop, shorttubebottom, longtubetop, longtubebottom]
##    e.g.
##    [14, 18, 7, 8]
##
##    second row describes the angular ranges one would like to mask between
##    units are in scattering angle in degrees
##    [[minangle1, maxangle1],[minangle2,maxangle2],[minangle3,maxangle3],[etc]]
##    e.g.
##    [[0,5], [32.75,33.75], [110,125]]
##
##    Remaining lines describe the packs, detectors and pixels to mask
##
##
##################################################

def nexus_mask_builder(instrument,runnum,maskparamsfile,filename="default"):

    import nxs

    # take care of instrument specific book-keeping
    if (instrument == 'ARCS'):
        shortlen = 0.5 # length below which a tube is considered short
        nbanks = 115   # total number of packs (banks ala Nexus)
    if (instrument == 'CNCS'):
        shortlen = 0.5
        nbanks = 50
    if (instrument == 'SEQ'):
        shortlen = 0.5
        nbanks = 113

    #read the maskparamsfile
    #print maskparamsfile
    tubeendarr,anglearr,droppacks = parsemaskparams(maskparamsfile)

    #print droppacks

    # start the mask file
    # only write text to the mask file after each subroutine is
    # finished.

    #find the file
    findnx_str = 'findnexus -i %s %d' %(instrument, runnum)
    filestr = Popen([findnx_str],shell=True,stdout=PIPE).communicate()[0]
    filestr=filestr.strip('\n')
    fid = nxs.open(filestr,'r')

    #mask the ends of the packs
    lbt = tubeendarr[0]
    lbb = tubeendarr[1]
    sbt = tubeendarr[2]
    sbb = tubeendarr[3]
    masktext1 = maskends(fid, nbanks,shortlen, lbt, lbb, sbt, sbb)


    #mask the scattering angle ranges    
    #loop through the number of sets of value for angle masking
    masktext2 = ''
    for angles in anglearr:
        lowerangle = angles[0]
        upperangle = angles[1]
        masktext2 += maskanglerange(fid, nbanks,lowerangle,upperangle)



    #mask the packs, tubes and pixels in the droppacks text.
    masktext3 = ''
    masktext3 = parsemasklist(droppacks)

    




    #concatenate the two mask strings together and save to a file
    masktext1 += masktext2
    masktext1 += masktext3

    maskfile = open(filename,'w')
    maskfile.write(masktext1)
    maskfile.close()



##################################################
# maskends
##################################################
# determines if the ends of each tube in a bank need to be masked or not
# returns mask text
#
# fileid = file id opend via nxs.open command prior to calling subroutine
# nbanks (number of banks in spectrometer, numbered starting from 1)
# lbt (long bank top)
# lbb (long bank bottom)
# sbt (short bank top)
# sbb (short bank bottom)
#
# Assumes 8 tubes per bank for all instruments (numbered 0-7).
# Assumes 128 pixels per tube for all instruments numbered 0-127
def maskends(fileid, nbanks,shortlen, lbt, lbb, sbt, sbb):


    numpixels=127
    numtubes = 7
    
    masktext = ""

    #loop through all of the banks
    bank = 1
    while (bank <= nbanks):
        #get the sizearray
        fileid.openpath('/entry/instrument/bank'+str(bank)+
                                   '/origin/shape/size')
        sizearray=fileid.getdata()

        #reset the tubenumber
        tubenum = 0

        #check if the sizearray is not short
        if (sizearray[1] > shortlen):
            # increment over the tubenumbers
            while (tubenum <= numtubes):
                #increment over the pixel numbers starting from bottom
                pixelnum = 0
                while (pixelnum < lbt):
                    masktext += "bank"+str(bank)+"_"+str(tubenum)+"_"+str(pixelnum)+"\n"
                    pixelnum += 1

                #now increment over the pixel numbers starting from the top
                pixelnum = 127
                while (numpixels-pixelnum < lbb):
                    masktext += "bank"+str(bank)+"_"+str(tubenum)+"_"+str(pixelnum)+"\n"
                    pixelnum -= 1
        
                tubenum+=1

        else:
            # increment over the tubenumbers
            while (tubenum <= numtubes):
                #increment over the pixel numbers starting from bottom
                pixelnum = 0
                while (pixelnum < sbt):
                    masktext += "bank"+str(bank)+"_"+str(tubenum)+"_"+str(pixelnum)+"\n"
                    pixelnum += 1

                #now increment over the pixel numbers starting from the top
                pixelnum = 127
                while (numpixels-pixelnum < sbb):
                    masktext += "bank"+str(bank)+"_"+str(tubenum)+"_"+str(pixelnum)+"\n"
                    pixelnum -= 1
        
                tubenum+=1
        
        bank = bank + 1
    



    return masktext

#########################################################
## maskanglerange
########################################################
# determines if pixel is within specified angular range
# returns mask text
#
# if pixels are between these two angles then mask them.
#
# fileid = file id opend via nxs.open command prior to calling subroutine
# nbanks (number of banks in spectrometer, numbered starting from 1)
# lowerangle of range to throw away (degrees)
# upperangle of range to throw away (degrees).
#
# Assumes 8 tubes per bank for all instruments (numbered 0-7).
# Assumes 128 pixels per tube for all instruments numbered 0-127
def maskanglerange(fileid, nbanks,lowerangle,upperangle):


    numpixels=127
    numtubes = 7
    
    masktext = ""

    #loop through all of the banks
    bank = 1
    while (bank <= nbanks):
        #get the sizearray
        fileid.openpath('/entry/instrument/bank'+str(bank)+
                                   '/polar_angle')
        ## theta arrray is 8 by 128
        thetaarray=fileid.getdata()

        #reset the tubenumber
        tubenum = 0

        #increment over the tubes in thetaarray.
        while (tubenum <= numtubes):
                #increment over the pixel numbers starting from bottom
                pixelnum = 0
                while (pixelnum <= numpixels):

                    thetavalue = thetaarray[tubenum,pixelnum]*180.0/3.14159
                    
                    #Test the scattering angle.
                    if (thetavalue > lowerangle and thetavalue < upperangle):
                        masktext += "bank"+str(bank)+"_"+str(tubenum)+"_"+str(pixelnum)+"\n"

                        
                    pixelnum += 1
        
                tubenum+=1
                
        bank += 1

    return masktext



#####################################################
## parsemaskparams
#####################################################
## parses the parameter file used to generate the mask file
## 
def parsemaskparams(filetoread):

    tubeendarr = []
    anglerangearr = []
    droppacks = []

#    print filetoread
    inputfile = open(filetoread,"r")

    # read the first two lines of the file to get the range.
    tubeendarr = inputfile.readline()
    anglerangearr = inputfile.readline()



    # read the remaining lines of the file to mask packs tubes and pixels.
    for remaining in inputfile:
            t1 = remaining.rstrip('\n')
            droppacks.append(t1)


    #clean up the first two lines.
    tubeendarr = tubeendarr.replace('\n','')
    anglerangearr = anglerangearr.replace('\n','')

    tubeendarr = tubeendarr.replace('[','')
    tubeendarr = tubeendarr.replace(']','')
    anglerangearr = anglerangearr.replace('[','')
    anglerangearr = anglerangearr.replace(']','')

    tubeendarr = string.split(tubeendarr,',')
    tubefloats = []
    for elem in tubeendarr:
        tubefloats.append(float(elem))

    tubeendarr = tubefloats

    anglerangearr = string.split(anglerangearr,',')
    anglefloats = []
    for elem in anglerangearr:
        anglefloats.append(float(elem))

    anglerangearr = anglefloats

    #reshape the anglerangearr
    anglerangearr = array(anglerangearr)
    anglerangearr = reshape(anglerangearr,(len(anglerangearr)/2,2))


    return tubeendarr, anglerangearr, droppacks



#parsemasklist,
# parses the listing of packs, detectors and pixels to mask
def parsemasklist(inputar):

     #outlist = []
     masktext = ""
     
     for val in inputar:
	     #no commas (only packs and hyphonated packs)
	     if val.count(',') == 0:
		     #look for a hyphen
		     #if there is a hyphen, find it and find the start and end values
		     if val.find("-") > 0 :
			     n1 = int(val[0:val.find("-")])
			     n2 = int(val[val.find("-")+1:len(val)])
			     list1 = range(n1,n2+1)
			     for element in list1:
				     for tube in range(7+1):
					     for pixel in range(127+1):
                                                     masktext += "bank"+str(element)+"_"+str(tube)+"_"+str(pixel)+"\n"
		     else:
			     for tube in range(7+1):
				     for pixel in range(127+1):
                                             masktext += "bank"+str(int(val))+"_"+str(tube)+"_"+str(pixel)+"\n"
             #single comma
 	     if val.count(',') == 1:
                     packs = val[0:val.find(",")]
 		     tubes = val[val.find(",")+1: len(val)]

 		     #look for a hyphen in packs
 		     if packs.find("-") > 0:
 			     pack1 = int(packs[0:packs.find("-")])
 			     pack2 = int(packs[packs.find("-")+1:len(packs)])
 			     packs = range(pack1,pack2+1)
 		     else: packs = range(int(packs),int(packs)+1)

 		     #look for a hyphen in tubes
 		     if tubes.find("-") > 0:
 			     tube1 = int(tubes[0:tubes.find("-")])
 			     tube2 = int(tubes[tubes.find("-")+1:len(tubes)])
 			     tubes = range(tube1,tube2+1)
 		     else: tubes = range(int(tubes),int(tubes)+1)

 		     for pack in packs:
			     for tube in tubes:
 				     for pixel in range(127+1):
 					     masktext += "bank"+str(pack)+"_"+str(tube)+"_"+str(pixel)+"\n"

             #two commas
	     if val.count(',') == 2:

		     packs = val[0:val.find(",")]
		     tubes = val[val.find(",")+1:val.rfind(",")]
		     pixels = val[val.rfind(",")+1:len(val)]

		     #look for a hyphen in packs
		     if packs.find("-") > 0:
			     pack1 = int(packs[0:packs.find("-")])
			     pack2 = int(packs[packs.find("-")+1:len(packs)])
			     packs = range(pack1,pack2+1)
		     else: packs = range(int(packs),int(packs)+1)

		     #look for a hyphen in tubes
		     if tubes.find("-") > 0:
			     tube1 = int(tubes[0:tubes.find("-")])
			     tube2 = int(tubes[tubes.find("-")+1:len(tubes)])
			     tubes = range(tube1,tube2+1)
		     else: tubes = range(int(tubes),int(tubes)+1)


		     #look for a hyphen in pixels
		     if pixels.find("-") > 0:
			     pixel1 = int(pixels[0:pixels.find("-")])
			     pixel2 = int(pixels[pixels.find("-")+1:len(pixels)])
			     pixels = range(pixel1,pixel2+1)
		     else: pixels = range(int(pixels),int(pixels)+1)



		     for pack in packs:
			     for tube in tubes:
				     for pixel in pixels:
                                             masktext += "bank"+str(pack)+"_"+str(tube)+"_"+str(pixel)+"\n"


     return masktext



# reads a maskfile that has the syntax of
#bankXX_Y_Z, where XX Y and Z are pack, tube, and pixel numbers
# the output is a coordinatefile
def nexus_mask_reader(instrument,runnum,maskfile,filename="default"):










    
