import re
from PyQt4.QtCore import *
from PyQt4.QtGui import *
from PyQt4 import QtGui
import ui_ViewNexDlg
import os
import nxs

class ViewNexDlg(QDialog, ui_ViewNexDlg.Ui_ViewNexDlg):
    
    instrument = ''
    
    def __init__(self, parent=None):
        super(ViewNexDlg, self).__init__(parent)
        self.setupUi(self)

    def on_runInfo_textEdited(self, runnum):
        self.index = 0

    def runnum(self):
        return self.__runnum

    def instrumentInfo_valueChanged(self, inst):
        return self.apply
    
    def on_searchButton_clicked(self):
        print 'in on_searchButton_clicked'
        #get instrument selected
        self.instrument = self.instrumentInfo.currentText()
        #get run number defined
        run_number = self.runInfo.text()
        result = os.popen('findnexus -i '+ str(self.instrument) + ' ' + str(run_number)).read()
        self.fullfilenameInfo.setText(result)
     
        #check that nexus is a real file
        result = result.split('\n')
        _file = os.path.expanduser(result[0])
        if os.path.exists(_file):
            self.retrieve_metadata(_file)

    def retrieve_metadata(self,filename):
        
        file=nxs.open(str(filename),'r')
        entry = 'entry'
        if (self.instrument == 'REF_M'):
            entry = 'entry-Off_Off'
        file.opengroup(entry,'NXentry')
        
        #start time
        file.opendata('start_time')
        start_time = str(file.getdata())
        file.closedata()
        self.timestartinfo.setText(start_time)

        #duration
        file.opendata('duration')
        duration = str(file.getdata())
        file.closedata()
        self.durationinfo.setText(duration)

        #proton charge
        file.opendata('proton_charge')
        proton_charge = str(file.getdata())
        file.closedata()
        self.protonchargeinfo.setText(proton_charge)

        #total counts
        file.opendata('total_counts')
        total_counts = str(file.getdata())
        file.closedata()
        self.totalcountsinfo.setText(total_counts)

        #slit1
        file.opendata('/entry/instrument/aperture1/s1t/value')
        s1t = str(file.getdata())
        units = file.getattr('units',10,'char')
        file.closedata()
        _s1t = float(s1t)

        file.opendata('/entry/instrument/aperture1/s1b/value')
        s1b = str(file.getdata())
        file.closedata
        _s1b = float(s1b)
        
        _s1 = _s1t - _s1b
        self.slit1h.setText(str(_s1) + ' ' + units)
        
#        file.opendata('/entry/instrument/aperture1/s1l/value')
#        s1l = str(file.getdata())
#        file.closedata
#        file.opendata('/entry/instrument/aperture1/s1r/value')
#        s1r = str(file.getdata())
#        file.closedata
#
#        #slit2
#        file.opendata('/entry/instrument/aperture2/s2t/value')
#        s2t = str(file.getdata())
#        file.closedata()
#        file.opendata('/entry/instrument/aperture2/s2b/value')
#        s2b = str(file.getdata())
#        file.closedata
#        file.opendata('/entry/instrument/aperture2/s2l/value')
#        s2l = str(file.getdata())
#        file.closedata()
#        file.opendata('/entry/instrument/aperture2/s2r/value')
#        s2r = str(file.getdata())
#        file.closedata
#
#        #slit3
#        file.opendata('/entry/instrument/aperture3/s3t/value')
#        s3t = str(file.getdata())
#        file.closedata()
#        file.opendata('/entry/instrument/aperture3/s3b/value')
#        s3b = str(file.getdata())
#        file.closedata
#        file.opendata('/entry/instrument/aperture3/s3l/value')
#        s3l = str(file.getdata())
#        file.closedata()
#        file.opendata('/entry/instrument/aperture3/s3r/value')
#        s3r = str(file.getdata())
#        file.closedata
#
#        #slit4
#        file.opendata('/entry/instrument/aperture4/s4t/value')
#        s4t = str(file.getdata())
#        file.closedata()
#        file.opendata('/entry/instrument/aperture4/s4b/value')
#        s4b = str(file.getdata())
#        file.closedata
#        file.opendata('/entry/instrument/aperture4/s4l/value')
#        s4l = str(file.getdata())
#        file.closedata()
#        file.opendata('/entry/instrument/aperture4/s4r/value')
#        s4r = str(file.getdata())
#        file.closedata
#       
#        #theta
#        file.opendata('/instrument/bank1/Theta/readback')
#        theta = str(file.getdata())
#        file.closedata()
#        self.tthetainfo.setText(theta)

        

        
        
        #do the same for all the other ones
        
        #WORK TO DO HERE
        
        
        file.close()

    def on_browseButton_clicked(self):
        file_path = os.path.expanduser("~/")
        oFiles = QtGui.QFileDialog.getOpenFileNames(self, "Select a NeXus file",
                                            file_path, "NeXus files (*.nxs)")
        for file in oFiles:
            self.retrieve_metadata(file)
            return
        
if __name__ == "__main__":
    import sys
    
    app = QApplication(sys.argv)
    form = ViewNexDlg()
    form.show()
    app.exec_()
    
