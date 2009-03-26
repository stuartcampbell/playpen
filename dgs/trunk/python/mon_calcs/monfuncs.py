"""
M. B. Stone
Monitor curve fitting functions
October/November 2008

Functions called in moncalc.py routines for curve fitting the
beam monitor data

"""


from scipy import special
from numpy import exp
from numpy import *
from scipy import *

#---GAUSS---
#
# gaussian function
# p[0] = bg
# p[1] = amplitude
# p[2] = center
# p[3] = sigma
def gauss(x, p):

    return p[0] + p[1]*exp(-1.0*((x-p[2])**2.0)/(2.0*p[3]*p[3]))

def residualgauss(p, y, x):
    err = y - gauss(x,p)
    return err

#---ICPULSE---
#
# Ikeda-Carpenter, modified pulse shape
# p[0] = bg
# p[1] = alpha
# p[2] = nu
# p[3] = amp
# p[4] = loc

def ICpulse(x, p):

    alpha = p[1]
    nu = p[2]
    amp = p[3]
    bg = p[0]
    loc = p[4]

    peakoff = nu/alpha
    timeval = loc + peakoff - x

    val = bg + amp*alpha/(special.gamma(nu+1)) * ((alpha*timeval)**(nu))*exp(-alpha*timeval)

 
    bval = where(x > (loc + peakoff))
    val[bval] = 0.0 + bg

 
    return val



def residualICpulse(p, y, x):
    err = y - ICpulse(x, p)
    return err


