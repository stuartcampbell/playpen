#!/usr/bin/python

from numpy import *
import reduction.interactive as ri
from xml.dom import minidom
from scipy import *
from scipy.optimize import leastsq
from monfuncs import *

#leastsq(func, x0, args=(), Dfun=None, full_output=0, col_deriv=0, ftol=1.49012e-08, xtol=1.49012e-08, gtol=0.0, maxfev=0, epsfcn=0.0, factor=100, diag=None, warning=True)

"""
M. B. Stone
Monitor calculations
October/November 2008

Program which loads beam monitor data from two beam monitors,
calculates and returns quantities of interest.

Prior to beginning use of these routines one needs to do
the following
$source ~lj7/arcs-event-mode-reduction.sh
or whatever the current source directory for Dr.Chops functions
is currently in use.

Example Usage (moncalc.py is in the current directory):
$source ~lj7/arcs-event-mode-reduction.sh
$python
>>>import moncalc as mc
>>>path = "/SNS/users/5us/data/SNS/ARCS/IPTS-1277/15"
>>>a1 = mc.moncalc(path,'ARCS',1260,11.825,18.5,60,480)


!!!! - further developments needed-
I need to add weighting to the curve fitting routines, for the
gaussian and IC fit functions.
!!!!


INPUT:
path - path to the data directory
runnum - the run number
d1mon - the distance from the face of moderator to monitor1
      - 11.825 m ARCS
d2mon - the distance from the face of moderator to monitor2
      - 18.5 m ARCS
nomEi - the nominal incident energy of the measurement
freq - the frquency used for the measurement (only for book-keeping)
OPTIONAL INPUT with DEFAULTS:
emissioncor=[128.5,-0.5255], values of the emmission time function to
            correct the preliminary calculation of peak location for
            emmisiontime (defaults are for ARCS)
peakrange=250, value in microseconds before and after the peak which
            are used for the calculation
bgpnts = 25, number of points to use for the background subtraction
            in peak determination.


OUTPUT:
runnum, nomEi,freq,Ei1,dEi1,t01,dt01,Ei2,dEi2,t02,dt02,Ei3,dEi3,t03,dt03, peakar1[0], peakar1[1], peakar1[2],peakar1[3], peakar1[4], peakar1[5], peakar1[6], peakar1[7], peakar1[8], peakar1[9],peakar1[10],peakar2[0], peakar2[1],peakar2[2], peakar2[3], peakar2[4], peakar2[5], peakar2[6], peakar2[7], peakar2[8], peakar2[9],peakar2[10], I1, trun, gaussfit[0], gaussfit[1], gaussfit[2], gaussfit[3], ICfit[0], ICfit[1], ICfit[2], ICfit[3], ICfit[4], gaussfit2[0], gaussfit2[1], gaussfit2[2], gaussfit2[3]
runnum - the input run number
nomEi - the nominal incident energy of the measurement
freq  - the frequency for the measurement
Ei1- the determined incident energy (fitting peaks 1 Gaussian, 2 IC form)
dEi1 - error in incident energy
t01 - the emmision time
dt01 - error in emmision time
Ei2- the determined incident energy (absolute peak value)
dEi2 - error in incident energy
t02 - the emmision time
dt02 - error in emmision time
Ei3- the determined incident energy (center of mass of peak)
dEi3 - error in incident energy
t03 - the emmision time
dt03 - error in emmision time
peaktime - monitor 1 peak (center of mass)
errtime - monitor 1 errror in peaktimet
maxinttime - value of time for the maximum counts in range
totalcounts - total counts in monitor 1 range
errtotalcounts - error in total counts for monitor 1
peakint - peak intensity (maximum)
dpeakint - error in the peak intensity
fwhm - fwhm of peak in microseconds
variance - of peak 1
skewness - of peak 1
peaktime - monitor 2 peak (center of mass)
errtime - monitor 2 errror in peaktimet
maxinttime - value of time for the maximum counts in range
totalcounts - total counts in monitor 2 range
errtotalcounts - error in total counts for monitor 2
peakint - peak intensity (maximum) 2
dpeakint - error in the peak intensity 2fwhm - fwhm of peak in microseconds 2
variance - of peak 2
skewness - of peak 2
beam current (Coulombs)
time of measurement (seconds)
Gaussian fit params, peak 1 (bg, amp, center, sigma)
ICfit params, peak 2 (bg, alpha, nu, amp, loc)
Gaussian Fit params, peak 2 (bg, amp, center, sigma)


"""
# Path for example for users, need to access down to go down
# to the sample number directory.
#       "/SNS/users/5us/data/SNS/ARCS/2008_3_18_SCI/15"
#
# Path for example for DFS access "/ARCS-DAS-FS/2008_3_18_SCI"
#
def moncalc(path, instrument ,runnum, d1mon, d2mon, nomEi, freq, emissioncor=[128.5,-0.5255],peakrange=250, bgpnts = 25):
   result = []

   #currently fileroot is set for running in users directories
   #fileroot for DFS access
   #fileroot = path+"/ARCS_"+str(runnum)+"/"
   #
   #fileroot for users.
   fileroot = path+"/"+str(runnum)+"/preNeXus/"

   file1string = instrument + "_"+str(runnum)+"_bmon1_histo.dat"
   file2string = instrument + "_"+str(runnum)+"_bmon2_histo.dat"


   mon1 = monload(fileroot+file1string)
   mon2 = monload(fileroot+file2string)

   #estimate the time centers for the incident energies and the distances
   time1 = esttimerange(nomEi, d1mon, emissioncor = emissioncor)
   time2 = esttimerange(nomEi, d2mon, emissioncor = emissioncor)

   #determine the peak parameters
   # [compeaktime, errcomtime, timeval, totalcounts,
   #  errtotalcounts, peakint, dpeakint[0], fwhm, variance, skewness,bg]
   # Note that the errors are still error squared
   peakar1,int1,err1,time1 = calcpeak(mon1, time1, peakrange, bgpnts)
   peakar2,int2,err2,time2 = calcpeak(mon2, time2, peakrange, bgpnts)

   # sqrt statistics for errors
   err1 = [sqrt(x) for x in err1]
   err2 = [sqrt(x) for x in err2]
  
   #do the curve fitting for the first monitor peak (~Gaussian) using starting
   #parameters output from calcpeak.
   # p[0] = bg
   # p[1] = amplitude
   # p[2] = center
   # p[3] = sigma
   gaussparam = [peakar1[10], peakar1[4], peakar1[0], 2.35*peakar1[7] ]
   gaussfit,num = leastsq(residualgauss, gaussparam, args=(int1,time1),maxfev=2000)   

   #do the curve fitting for the second monitor peak
   # Ikeda-Carpenter, modified pulse shape
   # p[0] = bg
   # p[1] = alpha
   # p[2] = nu
   # p[3] = amp
   # p[4] = loc
   ICparam = [peakar2[10], 0.13, 1.7, peakar2[5]*20.0, peakar2[0]]
   ICfit,num = leastsq(residualICpulse, ICparam, args=(int2,time2),maxfev=2000)


   # Also fit the second monitor peak to a Gaussian
   gaussparam2 = [peakar2[10], peakar2[5], peakar2[0], 2.35*peakar2[7] ]
   gaussfit2,num2 = leastsq(residualgauss, gaussparam2, args=(int2,time2),maxfev=2000)   



   # THREE ways to compute the energy and emmsiontime
   # 1) Gaussian and IC fits (error in time is 1/4 of estimated fwhm)
   # 2) Absolute peak value  (error in time is 1/4 of estimated fwhm)
   # 3) Center of mass peak  (error in time is 1/4 of estimated fwhm) 

   # Method 1
   Ei1,t01,dEi1,dt01 = calcenergyto(gaussfit[2], peakar1[7]/4.0, ICfit[4], peakar2[7]/4.0, d1mon, d2mon)
    
   # Method 2
   Ei2,t02,dEi2,dt02 = calcenergyto(peakar1[2], peakar1[7]/4.0, peakar2[2], peakar2[7]/4.0, d1mon, d2mon)

   # Method 3
   Ei3,t03,dEi3,dt03 = calcenergyto(peakar1[0], peakar1[7]/4.0, peakar2[0], peakar2[7]/4.0, d1mon, d2mon)

   #Get the beam current and the length of time of the run.
   # Note, these functions rely on fixed format of these runinfo and cvinfo
   # xml files.
   I1 = getbeamcurrent(fileroot+instrument + "_"+str(runnum)+"_runinfo.xml")
   trun =  getbeamtime(fileroot+instrument + "_"+str(runnum)+"_cvinfo.xml")

   return [runnum, nomEi,freq,Ei1,dEi1,t01,dt01,Ei2,dEi2,t02,dt02,Ei3,dEi3,t03,dt03, peakar1[0], peakar1[1], peakar1[2],peakar1[3], peakar1[4], peakar1[5], peakar1[6], peakar1[7], peakar1[8], peakar1[9],peakar1[10],peakar2[0], peakar2[1],peakar2[2], peakar2[3], peakar2[4], peakar2[5], peakar2[6], peakar2[7], peakar2[8], peakar2[9],peakar2[10], I1, trun, gaussfit[0], gaussfit[1], gaussfit[2], gaussfit[3], ICfit[0], ICfit[1], ICfit[2], ICfit[3], ICfit[4], gaussfit2[0], gaussfit2[1], gaussfit2[2], gaussfit2[3] ]



#---ESTTIMERANGE---
#estimate the time of flight range for a incident energy and distance
#input:
#	nomei - nominal incident energy in meV
#	dist  - distance from moderator face to location of interest (meters)
#	emissioncor - if three parameters listed, then corect for the emissiontime 
#		     using the functional form in the emissiontime function
#output:
#	timeofflight - in microseconds
def esttimerange(nomEi, dist, emissioncor = []):
    from math import sqrt

    meVperjoule = 6.24150974E21 # meV/Joule
    massneutron = 1.6749286E-27 # kg
    velocity = sqrt(2.0/massneutron*nomEi/meVperjoule)
    timeof = dist/velocity*1.0E6 # microseconds

    et = 0.0
    if len(emissioncor) != 0:
        et = emissiontime(nomEi, emissioncor[0], emissioncor[1])

    return timeof + et

#---EMISSIONTIME---
#return the emmision time given the three parameters determined
#apriori for characterization of the moderator spectrum
#input:
#	p1 -
#	p2 -
#	p3 -
#output:
#	emmisiontime - in microseconds
def emissiontime(nomEi, p1, p2):

    timeofemission =  p1*nomEi**(p2)

    return timeofemission



#--- MONLOAD ---
# loads the monitor data and returns histogram h
# one will use 
def monload(mon1file):


    from arcseventdata.monitorData import readHistogram
    h = readHistogram( mon1file )

    return h



#-----getbeamcurrent----
# get the beam current in coloumbs.
#
# runinfofile is a string of the directory and filename of the
# runinfo.xml file
# Note, this functions rely on fixed format of the runinfo.xml file
def getbeamcurrent(runinfofile):
	r1 = runinfofile
	xmldoc = minidom.parse(r1)
	listval = xmldoc.getElementsByTagName('PCurrent')
	current = float(listval[0].firstChild.data)/1.0E12

	return current



#-----getbeamtime----
# get the beam time in seconds.
#
# cvinfofile is a string of the directory and filename of the
# cvinfo.xml file
# Note, this function relys on fixed format of the cvinfo.xml files
def getbeamtime(cvinfofile):
	xmldoc = minidom.parse(cvinfofile)
	a1 = xmldoc.childNodes[0].childNodes[1].childNodes[3]
	a = a1.attributes["value"]
	time = float(a.value)

	return time




#--- CALCPEAK ---
# calculate peak parameters
#
# input:
#	mon - monitor histogram with mon.I, mon.E2, and mon.tof structure
#	timeval - estimated peak location in microseconds
#	plusminusrange - range of acceptable times around proposed peak loc for calc
#	bgpnts - number of points on the starting and ending range which are
#		used for bg subtraction in calculation of peak parameters
#
# output:
#	peaktime - time peak from first moment technique
#	errpeaktime - error in peak time
#       totalcounts - total counts in the range
#       errtotalcounts
#       peakint
#       dpeakint
#       fwhm - approximate fwhm in microseconds.
#       variance
#       skewness
#       bg
#       arrays for int, dint and tof.
#
#
def calcpeak(mon, timeval, plusminusrange, bgpnts):
	

	intrange = mon.I[(mon.tof<(timeval+plusminusrange)) & (mon.tof>(timeval-plusminusrange))]
	timerange = mon.tof[(mon.tof<(timeval+plusminusrange)) & (mon.tof>(timeval-plusminusrange))]
	errvals = []

	#find the peak position in the initial range,
        #  then set the range plus or minus on this
	#  true peak position.
	timeval = findpeak(intrange, timerange)

	intrange = mon.I[(mon.tof<(timeval+plusminusrange)) & (mon.tof>(timeval-plusminusrange))]
	timerange = mon.tof[(mon.tof<(timeval+plusminusrange)) & (mon.tof>(timeval-plusminusrange))]
	errvals = []
        
        #Keep errors as error squared
	for x in intrange:
		errvals.append(x)

        if (bgpnts > 0):
        #subtract the mean bg and also account for error in bg.
             bgar1 = intrange[0:bgpnts]
             bgar2 = intrange[len(intrange)-bgpnts:len(intrange)]
             bg = concatenate((bgar1,bgar2))
             meanbg = mean(bg)
             intrange = [x-meanbg for x in intrange]

             errbg = sqrt(sum(bg))/len(bg)

        else:
             errbg = 0.0
	     meanbg = 0.0

        errvalsnobg = errvals
        errvals = [x+errbg**2 for x in errvals]


        # convert to arrays to be able to use array multiplication from numpy
        intrange = array(intrange)
        timerange = array(timerange)
        errvals = array(errvals)

        #Find the center of mass of the distribution
        aval = sum(intrange*timerange)
        bval = sum(intrange)

        errasq = sum(errvals*timerange*timerange)
        errbsq = sum(errvals)

        compeaktime = aval/bval
        errcomtime = sqrt(errasq/bval/bval + errbsq*aval*aval/bval**4)
	
        totalcounts = sum(intrange)
        errtotalcounts = sqrt(sum(errvals))

        # find the peak intensity and ~FWHM
        peakint = max(intrange)
        dpeakint = sqrt(errvals[where(intrange==max(intrange))])

        timefwhm1 = max(timerange[(intrange<(0.5*peakint)) & (timerange < timeval)])
        timefwhm2 = min(timerange[(intrange<(0.5*peakint)) & (timerange > timeval)])

        fwhm = timefwhm2 - timefwhm1

        intrangenobg = [x+meanbg for x in intrange]
	probint = intrangenobg/sum(intrangenobg)
        variance = findvariance(intrangenobg, errvals, timerange, timeval)
	skewness = findskewness(intrangenobg, errvals, timerange, timeval)

        output = [compeaktime, errcomtime, timeval, totalcounts, errtotalcounts, peakint, dpeakint[0], fwhm, variance, skewness,meanbg]

	return output,intrangenobg,errvalsnobg,timerange




#---FINDPEAK---
#
#Simply finds the peak time for a given set of intensities and times
# this peak time is just the time of the maximum intensity.
def findpeak(intvals, timevals):
	

	intvals = array(intvals)
	timevals = array(timevals)

	peakvalue = where(intvals == max(intvals))
	timepeak = timevals[peakvalue]

	return timepeak[0]


#---FINDVARIANCE---
#
#Find the variance of a distibtuion
def findvariance(intvals, dintvals, timevals, meantime):

	probint = intvals/sum(intvals)
        variance = sqrt(sum(probint*(timevals-meantime)**2))
	
	return variance

#---FINDSKEWNESS---
#
#Find the skewness of a distribution
def findskewness(intvals, dintvals, timevals, meantime):

	variance = findvariance(intvals, dintvals, timevals, meantime)
	probint = intvals/sum(intvals)
	mu3 = sum(probint*(timevals-meantime)**3.0)

	skewness = mu3/variance**3.0

	return skewness


#---CALCENERGYTO---
# 
# Calculate the energy and t0 time
def calcenergyto(time1, errtime1, time2, errtime2, d1, d2):
   

   timearr = array([time1,time2])
   distarr = array([d1, d2])
   dtimearr = array([errtime1, errtime2])


   slope, intercept, dslope, dintercept = Weightedfitline(distarr,timearr,dtimearr)

   emissiontime = intercept
   dtime = dintercept

   velocity = 1.0/slope*1.0E6   # m/s
   dvelocity = dslope/slope/slope*1.0E6
   
   mneut = 1.6749286E-27

   energy = 0.5*mneut*velocity*velocity*6.24150974E21
   denergy = 0.5*2.0*mneut*6.24150975E21*dvelocity*velocity

   energyarr = [energy,emissiontime,denergy,dtime]

   return energyarr



#---FitLine---
# Calculates the slope and intercept for a linear regression
# also returns the error bars and chi sqr.
#
# from "An Introduction to Error Analysis", J. R. Taylor
def fitline(xvals,yvals):

   xvals = array(xvals)
   yvals = array(yvals)

   denom = float(len(yvals)*sum(xvals*xvals)-(sum(xvals))**2)

   intercept = (sum(xvals*xvals)*sum(yvals) - sum(xvals)*sum(xvals*yvals))/denom
   slope = (len(yvals)*sum(xvals*yvals) - sum(xvals)*sum(yvals))/denom

   sum1 = 0
   for i in range(len(yvals)):
      sum1 = sum1 + (yvals[i] - intercept - slope*xvals[i])**2

   dval = 2.0
   if len(yvals) == 2:
      dval = 1.0

   sigy = sqrt((1.0/(len(yvals)-dval))*sum1)
   sigslope = sigy*sqrt(float(len(yvals))/denom)
   sigintercept = sigy*sqrt(float(sum(xvals*xvals))/denom)


   outputarr = [slope, intercept, sigslope, sigintercept]

   return outputarr

#---WeightedFitLine---
# Calculates the slope and intercept for a linear regression
# also returns the error bars
#
# from "An Introduction to Error Analysis", J. R. Taylor
def Weightedfitline(xvals,yvals,erryvals):

   xvals = array(xvals)
   yvals = array(yvals)
   erryvals = array(erryvals)
   weights = 1.0/erryvals/erryvals
   
   denom = sum(weights)*sum(weights*xvals*xvals) - (sum(weights*xvals))**2

   
   
   intercept = (sum(weights*xvals*xvals)*sum(weights*yvals) - sum(xvals*weights)*sum(weights*xvals*yvals))/denom
   slope = (sum(weights)*sum(weights*xvals*yvals) - sum(xvals*weights)*sum(weights*yvals))/denom


   sigslope =  sqrt(sum(weights)/denom)
   sigintercept = sqrt(sum(weights*xvals*xvals)/denom)


   outputarr = [slope, intercept, sigslope, sigintercept]

   return outputarr
