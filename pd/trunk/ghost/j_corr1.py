#!/bin/env python
import os
import sys
import pylab, numpy
"""
Plot POWGEN3 DATA
"""

#detector_list.txt file
det_lst = open ('ghost_list.txt', 'r')

#Read first line: number of modules
line = det_lst.readline()
sline = line.split()
nmod = int(sline[0])

#detector arrays
pix_st = numpy.zeros(nmod)
d_sp = numpy.zeros(nmod)
labels = ''

#ghost maps
maxGs = 16
GhostType = numpy.dtype([('pix','uint32'),('strength','float64')]) # event data format

GhostPks = numpy.memmap('GhostPks.dat',dtype=GhostType,mode='r',shape=(300000,maxGs))  #maxGs

ghost = GhostPks['pix']
strength = GhostPks['strength']

#read nmod data: pixel_id and d_space1
for i in range(nmod):
    line = det_lst.readline()
    sline = line.split()
    pix_st[i] = int(sline[0])
    labels += sline[1]+' '
    d_sp[i] = float(sline[2])
    

labels = labels.split()
print pix_st, labels, d_sp

#input run_number on command line after script name(35, 558, or 527)
run_number       = int(sys.argv[1])

instrument       = 'PG3'
dspacemap_file   = 'powgen_dspacemap.dat'
event_location = ("")
run_prefix = '%s_%s' % (instrument,run_number)

output_folder = ('output%i/' %(run_number))
os.system('mkdir -p '+output_folder)

nd2  =   11000  # number of d-space bins for output files
dmin =    0.1   # min d [A]
dmax =   2.0         #  max d [A]
ddcw = 0.0004
lgdmin = numpy.log(dmin)
lgdmax = lgdmin+(ddcw*nd2)

NeutronEvent = numpy.dtype([('tof','uint32'),('pix','uint32')]) # event data format
dspmap  = numpy.memmap(dspacemap_file,   dtype='float64',    mode='r')

totdhist = numpy.zeros(nd2) + 0.0   # correction histogram
totchist = numpy.zeros(nd2) + 0.0   # correction histogram

for i in range(nmod):
    event_file = "PG3_%d_neutron_event.dat" % run_number
    events  = numpy.memmap(event_file,       dtype=NeutronEvent, mode='r')
    print event_file
    
    pix = events['pix']
    tof = events['tof']

    # Select ROI
    tsel = (tof>=10) * (tof<5000000)   # All Tof

    ne =  numpy.size(pix)
    print labels[i], ' events = ', ne
    
    ib = (pix>=(pix_st[i])) * (pix<(1232+pix_st[i]))  # in bounds 
    
    # Here comes the beauty of numpy
    d   = numpy.log(dspmap[pix*ib]*tof) # calculate d (using tabulated Bragg formula) from 

    # Plot and output

    sel = tsel * ib
    dhist,dbins = numpy.histogram(d  [sel], bins=nd2, range=(lgdmin,lgdmax))
    dbins = numpy.exp(dbins)

    chist = numpy.zeros(nd2) + 0.0   # smoothed correction histogram
    zhist = numpy.zeros(nd2) + 0.0   # correction histogram
    dd = dbins[1]-dbins[0]

    # weighted histogram approach

    for g in range(maxGs):  #maxGs
        print labels[i], 'ghost set: ', g
        pix2 = ghost[pix,g]
        d2 = numpy.log(dspmap[pix2]*tof)
        ahist,dbins = numpy.histogram(d2, weights=strength[pix ,g], bins=nd2, range=(lgdmin,lgdmax))
        zhist += ahist
        
    # Average of 7 nearest neighbors
    smw = 3
    pts = 2*smw*1.0
    for k in range(smw,nd2-smw):
        chist[k] = sum(zhist[k-smw:k+smw+1])/pts
    chist = zhist


    cchist = dhist - chist

    dbins = numpy.exp(dbins)

    # totals
    totdhist += dhist
    totchist += chist
    

    inp_name = '%s_%i%s' %(labels[i],run_number,'.inp')
    inp_file = open(output_folder+inp_name, 'w')
    inp_file.write('d_space  y_obs  y_ghost  y_adult \n')
    for k in xrange(nd2):
        inp_file.write('%f %i %f %f \n' %(dbins[k], dhist[k], chist[k], dhist[k]-chist[k]))
    inp_file.close()

    del events
    

cchist = totdhist - totchist

pylab.figure(500)

#	green curve = Original observed data(totdhist)
#	blue curve = Ghost profile(totchist)
#	red curve = Corrected profile(cchist)
line1 = pylab.plot(dbins[:-1],totdhist,linestyle='steps')
line2 = pylab.plot(dbins[:-1],totchist,linestyle='steps')
line3 = pylab.plot(dbins[:-1],cchist,linestyle='steps')
pylab.figlegend((line1,line2,line3),('Original observed data','Ghost profile','Corrected profile'),'upper right')
pylab.xlabel('d [A]')
pylab.ylabel('sum = %i ' %(sum(cchist)))
pylab.title('Instrument:  %s  Run #%s   Module: %s' %(instrument,run_number, labels))


pylab.figure(600)

pylab.plot(dbins[:-1],cchist,linestyle='steps')
pylab.xlabel('d [A]')
pylab.ylabel('sum = %i ' %(sum(cchist)))
pylab.title('Instrument:  %s  Run #%s   Module: %s' %(instrument,run_number, labels))

pylab.show()  


inp_name = '%i_bk1%s' %(run_number,'.out')
inp_file = open(output_folder+inp_name, 'w')
inp_file.write('d_space  y_obs  y_ghost  y_adult \n')
for k in xrange(nd2):
    inp_file.write('%f %i %f %f \n' %(dbins[k], totdhist[k], totchist[k], cchist[k]))
inp_file.close()

inp_name = '%i_bk1%s' %(run_number,'.inp')
inp_file = open(output_folder+inp_name, 'w')
inp_file.write('TOF  y_corr  y_inc \n')
for k in xrange(nd2):
    inp_file.write('%f %f %f \n' %(10000.0*dbins[k], cchist[k], 1000.0))
inp_file.close()




