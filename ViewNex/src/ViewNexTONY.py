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
        self.timestartInfo.setText(start_time)
        
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
    
