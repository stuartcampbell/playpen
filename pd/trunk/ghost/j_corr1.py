#!/bin/env python
import os
import sys
import pylab, numpy
"""
Plot POWGEN3 DATA
"""

def get_event_file_name(run_number, instrument, proposal, dfs_root):
    "Get event file name given run_number, instrumen name, proposal id and dfs_root directory"
    run_prefix = '%s_%s' % (instrument,run_number) 
    return os.path.join(dfs_root,proposal,run_prefix, '%s_neutron_event.dat' % run_prefix)


#detector_list.txt file
det_lst = open ('ghost_list.txt', 'r')
#det_lst = open ('bank2_list.txt', 'r')

#temp files for inspection
firstround = open ('firstround.txt','wt')
secondround = open ('secondround.txt','wt')  # with corrected difcs
list1 = open ('list1.out','wt') # of ghosts

#Read first line: number of modules
line = det_lst.readline()
sline = line.split()
nmod = int(sline[0])

#detector arrays
pix_st = numpy.zeros(nmod)
d_sp = numpy.zeros(nmod)
labels = ''
swa = numpy.zeros(nmod) + 5     # number of channels to sum over +-   ; need to read in per module
cscale = numpy.zeros(nmod) + 0.0

ncol = 154   # 154
#module arrays
difc = numpy.zeros(154)+0.1
odifc = difc.copy()             # observed difc
ndifc = difc.copy()             # new difc
gmatch = difc.copy()             # ghost match
perr = numpy.zeros(154)+0.0     # error in observed main peak position compared to diff for adjacent pixels
bk = numpy.zeros(154)
mpk = numpy.zeros(154)           # array for main pk intensity (counts)
mw = numpy.zeros(154)           # array for main pk widths. part of check

#modgs = numpy.zeros([154,154])+0.0  # module ghosts map (j,gpixid)

#ghost maps
maxGs = 16
GhostType = numpy.dtype([('pix','uint32'),('strength','float64')]) # event data format

#BraggPks = numpy.memmap('MainBraggPk.dat',dtype='float64',mode='w+',shape=(300000))
GhostPks = numpy.memmap('GhostPks.dat',dtype=GhostType,mode='r',shape=(300000,maxGs))  #maxGs


ghost = GhostPks['pix']
strength = GhostPks['strength']
#print ghost[155000:155010]




#read nmod data
for i in range(nmod):
    line = det_lst.readline()
    sline = line.split()
    pix_st[i] = int(sline[0])
    labels += sline[1]+' '
    d_sp[i] = float(sline[2])
    

labels = labels.split()
print pix_st, labels, d_sp

#run_number       = 35
run_number       = int(sys.argv[1])
expt_number = 3
if (run_number < 76):
    expt_number = 2

instrument       = 'PG3'
physicalmap_file = 'powgen_physicalmap.dat'
dspacemap_file   = 'powgen_dspacemap.dat'
#event_file       = get_event_file_name(run_number, instrument=instrument, 
#                        proposal='2009_2_11_SCI',dfs_root='/PG3-DAS-FS')
proposal ='2009_2_11_SCI/'
#event_location = ('%s%s%i%s%i%s' %('/SNS/PG3/', proposal, expt_number,'/', run_number, '/preNeXus/' ))
#event_location = ('%s%i%s' %('C:/PG_DATA/PG3_', run_number,'/' ))
event_location = ("")
run_prefix = '%s_%s' % (instrument,run_number)

#event_file = os.path.join(event_location, '%s_%s.rdat' % (run_prefix,labels[0]))



                          

#output_folder = ('/output/%i/' %(run_number))
output_folder = ('output\\%i\\' %(run_number))
os.system('mkdir '+output_folder)


ustics = 10   # number of DAS tics in usec                                 
nx   =   480  # number of x pixels
ny   =    80  # number of y pixels
np   = nx*ny  # total number of pixels
nt   =   300  # number of tof bins
nb   =   300  # number of d-space bins for plot
nd = 5000
nd2  =   11000  # number of d-space bins for output files
tmax = 200000.0*ustics #  max TOF [us]
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
    event_file = os.path.join(event_location, '%s_%s.rdat' % (run_prefix,labels[i]))
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
    #det = phymap[pix]     # convert electronics pixel into physical detector pixels using physical map
    d   = numpy.log(dspmap[pix*ib]*tof) # calculate d (using tabulated Bragg formula) from 

    # Plot and output

    sel = tsel * ib
    dhist,dbins = numpy.histogram(d  [sel], bins=nd2, range=(lgdmin,lgdmax), new=True)
    dbins = numpy.exp(dbins)
##    pylab.figure(200)
##    pylab.subplot(nmod,1,i+1)
##    pylab.plot(dbins[:-1],dhist,linestyle='steps')
##    pylab.xlabel('d [A]')
##    pylab.ylabel('sum = %i ' %(sum(dhist)))
##    pylab.title('Instrument:  %s  Run #%s   Module: %s' %(instrument,run_number, labels[i]))


    chist = numpy.zeros(nd2) + 0.0   # smoothed correction histogram
    zhist = numpy.zeros(nd2) + 0.0   # correction histogram
    dd = dbins[1]-dbins[0]


#    print chist




    # weighted histogram approach

    for g in range(maxGs):  #maxGs
        print labels[i], 'ghost set: ', g
        pix2 = ghost[pix,g]
        d2 = numpy.log(dspmap[pix2]*tof)
        
        ahist,dbins = numpy.histogram(d2, weights=strength[pix ,g], bins=nd2, range=(lgdmin,lgdmax), new=True)
        zhist += ahist
        
    smw = 3
    for k in range(smw,nd2-smw):
        chist[k] = sum(zhist[k-smw:k+smw+1])/(smw+smw+1.0)
    chist = zhist


    cchist = dhist - chist

##    pylab.figure(300)
##    pylab.subplot(nmod,1,i+1)
##
##    #pylab.plot(dbins[:-1],dhist,linestyle='steps')
    dbins = numpy.exp(dbins)
##    pylab.plot(dbins[:-1],chist,linestyle='steps')
##    #pylab.plot(dbins[:-1],cchist,linestyle='steps')
##    pylab.xlabel('d [A]')
##    pylab.ylabel('sum = %i ' %(sum(chist)))
##    pylab.title('Instrument:  %s  Run #%s   Module: %s' %(instrument,run_number, labels[i]))

##    # corrected profiles
##    pylab.figure(400)
##    pylab.subplot(nmod,1,i+1)
##
##    pylab.plot(dbins[:-1],cchist,linestyle='steps')
##    pylab.xlabel('d [A]')
##    pylab.ylabel('sum = %i ' %(sum(cchist)))
##    pylab.title('Instrument:  %s  Run #%s   Module: %s' %(instrument,run_number, labels[i]))

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

pylab.plot(dbins[:-1],totdhist,linestyle='steps')
pylab.plot(dbins[:-1],totchist,linestyle='steps')
pylab.plot(dbins[:-1],cchist,linestyle='steps')
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



firstround.close()

