#"useful functions"
from scipy import special
from scipy.signal import convolve
from numpy import exp, sqrt, pi, where, sum, array
import pylab
#---GAUSS---
#
# gaussian function
# p[0] = bg
# p[1] = amplitude
# p[2] = center
# p[3] = sigma
def gauss(x, p):
    "gauss(x,p)\n p[0] = bg\n p[1] = amplitude \n p[2] = center \n p[3] = sigma \n M. B. Stone \n October/November 2008"

    return p[0] + p[1]*exp(-1.0*((x-p[2])**2.0)/(2.0*p[3]*p[3]))
    
#---ICPULSE---
#
# Ikeda-Carpenter, modified pulse shape
# p[0] = bg
# p[1] = alpha
# p[2] = nu
# p[3] = amp
# p[4] = loc

def ICpulse(x, p):
    "Ikeda-Carpenter, modified pulse shape\n ICpulse(x, p)\n p[0] = bg\n p[1] = alpha\n p[2] = nu\n p[3] = amp\n p[4] = loc\n M. B. Stone \n October/November 2008"
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
def IC_Gauss_conv(x,p):
	"Ikeda-Carpenter, convolved with a gaussian\n bkg=p[0]\n"
	bkg=p[0]
	alpha = p[1]
	nu = p[2]
	amp = p[3]
	bg = p[0]
	loc = p[4]
	gamp=p[6]
	gsigma=p[5]
	#gamp=1.0/(sqrt(2*pi)*gsigma)
	gcent=(max(x)+min(x))/2.0
	ICprt=ICpulse(x,[0.0,alpha,nu,amp,loc])
	gaussprt=gauss(x,[0.0,gamp,gcent,gsigma])
	return bkg+convolve(ICprt,gaussprt,'same')

def res_IC_Gauss_conv(p,y,x):
     err=y-IC_Gauss_conv(x,p)
     return err

def peakstats(hist):
     "peakstats(hist) function that takes a 1d histogram of a single peak and returns the mean and the second moment of the peak"
     x=hist.axisFromId(1).binCenters()
     y=hist.I
     denom=y.sum()
     mean=(x*y).sum()/denom
     sec_mom=((x-mean)*(x-mean)*y).sum()/denom
     return [mean, sec_mom]
def save_ascii(hist,fname):
     "save_ascii(hist) save a histogram to a 3 column ascii file"
     x=hist.axisFromId(1).binCenters()
     y=hist.I
     err=sqrt(hist.E2)
     out=array([x,y,err])
     pylab.save(fname,out.transpose())
     
