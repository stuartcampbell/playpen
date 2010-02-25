# !/bin/env python
import os
import sys
import pylab, numpy
"""
Plot POWGEN3 DATA
"""
# input run_number on command line after script name(35, 558, or 527)
run_number       = int(sys.argv[1])
instrument       = 'PG3'

# input files
event_file = "PG3_%d_neutron_event.dat" % run_number
print event_file
dspacemap_file   = 'powgen_dspacemap.dat'
GhostPks_file    = 'GhostPks.dat'
ghost_list_file  = 'ghost_list.txt'

# create folder for output files
output_folder = ('output%i/' %(run_number))
os.system('mkdir -p '+output_folder)

# parameters that may be run dependent
pix_max = 1232    # appears to be the maximum number of pixels
tof_min = 10      # min tof
tof_max = 5000000 # max tof
nd2  =   11000    # number of d-space bins for output files
dmin =    0.1     # min d [A]
dmax =   2.0      # max d [A]
ddcw = 0.0004     # multiplies nd2 for max of histogram
lgdmin = numpy.log(dmin)   # min of histogram
lgdmax = lgdmin+(ddcw*nd2) # max of histogram
maxGs = 16          # number of ghost maps
maxGpix = 300000    # number of ghost map pixels

# detector_list.txt file
det_lst = open (ghost_list_file, 'r')

# Read first line: number of modules (Several modules are in a bank)
line = det_lst.readline()
sline = line.split()
nmod = int(sline[0])

# detector arrays
pix_st = numpy.zeros(nmod)
d_sp = numpy.zeros(nmod)
labels = ''

# read nmod data: pixel_id and d_space1 
# pixel_id is only used to check if events are in bounds
# d_sp and labels are only printed and not used
for i in range(nmod):
    line = det_lst.readline()
    sline = line.split()
    pix_st[i] = int(sline[0])
    labels += sline[1]+' '
    d_sp[i] = float(sline[2])

labels = labels.split()
print pix_st, labels, d_sp

# ghost maps
GhostType = numpy.dtype([('pix','uint32'),('strength','float64')]) # event data format
GhostPks = numpy.memmap(GhostPks_file,dtype=GhostType,mode='r',shape=(maxGpix,maxGs))  # maxGs
ghost = GhostPks['pix']
strength = GhostPks['strength']

NeutronEvent = numpy.dtype([('tof','uint32'),('pix','uint32')]) # event data format
dspmap  = numpy.memmap(dspacemap_file,   dtype='float64',    mode='r')

# initialize total histogram arrays
totdhist = numpy.zeros(nd2) + 0.0   # original observed histogram
totchist = numpy.zeros(nd2) + 0.0   # correction histogram

# histograms for each module calculated separately
for i in range(nmod):
    events  = numpy.memmap(event_file,       dtype=NeutronEvent, mode='r')
    
    pix = events['pix']
    tof = events['tof']

    # arrays of true if tof and pix is in bounds; false if not
    tsel = (tof>=tof_min) * (tof<tof_max)   # All Tof
    ib = (pix>=(pix_st[i])) * (pix<(pix_max+pix_st[i]))  # check if event is in bounds 

    # print number of events
    ne =  numpy.size(pix)
    print labels[i], ' events = ', ne
    
    
    # Here comes the beauty of numpy
    d   = numpy.log(dspmap[pix*ib]*tof) # calculate d (using tabulated Bragg formula) 

    # Histogram original observed data
    sel = tsel * ib # False if data is not in bounds
    dhist,dbins = numpy.histogram(d[sel], bins=nd2, range=(lgdmin,lgdmax))
    # exp of dbins since range was logarithmic
    dbins = numpy.exp(dbins)

    chist = numpy.zeros(nd2) + 0.0   # smoothed correction histogram
    zhist = numpy.zeros(nd2) + 0.0   # correction histogram

    # weighted histogram approach for ghost points; histogram done 16 times
    for g in range(maxGs):  #maxGs
        print labels[i], 'ghost set: ', g
        pix2 = ghost[pix,g]
        d2 = numpy.log(dspmap[pix2]*tof) # calculate d (using tabulated Bragg formula)
        # Histogram of ghost points
        ahist,dbins = numpy.histogram(d2, weights=strength[pix ,g], bins=nd2, range=(lgdmin,lgdmax))
        # Sum ghost histograms for all maxGs
        zhist += ahist
        
    # smooth with average of 7 points
#    smw = 3
#    pts = 2*smw*1.0
#    for k in range(smw,nd2-smw):
#        chist[k] = sum(zhist[k-smw:k+smw+1])/pts

    # not using smoothing since chist is overwritten
    chist = zhist

    # exp of dbins since range was logarithmic
    dbins = numpy.exp(dbins)

    # totals over modules
    totdhist += dhist
    totchist += chist
    
    # print intermediate histograms of module
    inp_name = '%s_%i%s' %(labels[i],run_number,'.inp')
    inp_file = open(output_folder+inp_name, 'w')
    inp_file.write('d_space  y_obs  y_ghost  y_adult \n')
    for k in xrange(nd2):
        inp_file.write('%f %i %f %f \n' %(dbins[k], dhist[k], chist[k], dhist[k]-chist[k]))
    inp_file.close()

    del events
    
# subtract Ghost histogram from Original observed histogram
cchist = totdhist - totchist

# integrate 3 histograms to check conservation of intensity
dbinsm1 = dbins[:-1]
trapd = numpy.trapz(totdhist, dbinsm1)
print 'Original observed data integral = ' ,trapd
trapc = numpy.trapz(totchist, dbinsm1)
print 'Ghost profile integral = ' ,trapc
trapcc = numpy.trapz(cchist, dbinsm1)
print 'Corrected profile integral = ' ,trapcc

# plot final histograms
pylab.figure(500)
# green curve = Original observed data(totdhist)
# blue curve = Ghost profile(totchist)
# red curve = Corrected profile(cchist)
line1 = pylab.plot(dbins[:-1],totdhist,linestyle='steps')
line2 = pylab.plot(dbins[:-1],totchist,linestyle='steps')
line3 = pylab.plot(dbins[:-1],cchist,linestyle='steps')
pylab.figlegend((line1,line2,line3),('Original observed data','Ghost profile','Corrected profile'),'upper right')
pylab.xlabel('d [A]')
pylab.ylabel('sum = %i ' %(sum(cchist)))
pylab.title('Instrument:  %s  Run #%s   Module: %s' %(instrument,run_number, labels))

# plot corrected profile(cchist)
pylab.figure(600)
pylab.plot(dbins[:-1],cchist,linestyle='steps')
pylab.xlabel('d [A]')
pylab.ylabel('sum = %i ' %(sum(cchist)))
pylab.title('Instrument:  %s  Run #%s   Module: %s' %(instrument,run_number, labels))

pylab.show()  

# print final histograms
inp_name = '%i_bk1%s' %(run_number,'.out')
inp_file = open(output_folder+inp_name, 'w')
inp_file.write('d_space  y_obs  y_ghost  y_adult \n')
for k in xrange(nd2):
    inp_file.write('%f %i %f %f \n' %(dbins[k], totdhist[k], totchist[k], cchist[k]))
inp_file.close()

# print corrected profile(cchist)
inp_name = '%i_bk1%s' %(run_number,'.inp')
inp_file = open(output_folder+inp_name, 'w')
inp_file.write('TOF  y_corr  y_inc \n')
for k in xrange(nd2):
    inp_file.write('%f %f %f \n' %(10000.0*dbins[k], cchist[k], 1000.0))
inp_file.close()




