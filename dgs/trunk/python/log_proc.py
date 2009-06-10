# routines to read an process runinfo files
#GEG 6.2009
#SNS
from xml.dom.minidom import *
def runinfo(instrument,proposal,runnum):
     """
       runinfo(instrument,proposal,runnum)
       instrument is the short insturment name, proposal is the poroposal ID, 
       runnum is the runnumber
       returns a dictionary containing info from the run info file
     """
     # possible instrument names
     posinst=set(['ARCS','SEQ','CNCS','BASIS'])
     if not set([instrument]).issubset(posinst):
          raise ValueError, "must be a short instrument name"
     instrun='%s_%i' %(instrument,runnum)
     filestr=r'/'+instrument+r'-DAS-FS/'+proposal+r'/'+instrun+r'/'+instrun+'_runinfo.xml'
     return read_info(filestr)
def read_info(filename):
      vainfo={}
      dat=parse(filename)
      akeys=dat.documentElement.attributes.keys()
      for aitem in akeys:
          vainfo[aitem]=dat.documentElement.attributes[aitem].value
       
      # get time and date
      di=dat.childNodes[0].childNodes[1]
      for idx in range(len(di.childNodes)):
           locName=di.childNodes[idx].localName
           if locName=='StartTime':
	        evalstr="vainfo['"+locName+"']= '%s'" %(di.childNodes[idx].childNodes[0].nodeValue)
		exec(evalstr)
	   if locName=='StopTime':
	        evalstr="vainfo['"+locName+"']= '%s'" %(di.childNodes[idx].childNodes[0].nodeValue)
		exec(evalstr)
      #operational info
      oi=dat.childNodes[0].childNodes[3]
      for idx in range(len(oi.childNodes)):
          locName=oi.childNodes[idx].localName
      	  if locName=='PCurrent':
	  	evalstr="vainfo['"+locName+"']= %s" %(oi.childNodes[idx].childNodes[0].nodeValue)
		exec(evalstr)
      #Sample info
      #dat.childNodes[0].childNodes[5]
      #General info
      gi=dat.childNodes[0].childNodes[7]
      for idx in range(len(gi.childNodes)):
          locName=gi.childNodes[idx].localName
          if locName=='Title':
	       evalstr="vainfo['"+locName+"']= '%s'" %(gi.childNodes[idx].childNodes[0].nodeValue)
	       exec(evalstr)
	  if locName=='Notes':
	       evalstr="vainfo['"+locName+"']= '%s'" %(gi.childNodes[idx].childNodes[0].nodeValue)
	       exec(evalstr)
	  
	       
      # Detector info
      dei=dat.childNodes[0].getElementsByTagName(u'DetectorInfo')[0].getElementsByTagName('Scattering')[0]
      timechannel={}
      akeys=dei.getElementsByTagName('NumTimeChannels')[0].attributes.keys()
      for aitem in akeys:
          timechannel[aitem]=dei.getElementsByTagName('NumTimeChannels')[0].attributes[aitem].value
      vainfo['timing']=timechannel	       
      return vainfo
