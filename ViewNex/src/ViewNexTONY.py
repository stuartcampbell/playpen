import re
from PyQt4.QtCore import *
from PyQt4.QtGui import *
import ui_ViewNexDlg
import os

class ViewNexDlg(QDialog, ui_ViewNexDlg.Ui_ViewNexDlg):
    
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
        instrument = self.instrumentInfo.currentText()
        #get run number defined
        run_number = self.runInfo.text()
        result = os.popen('findnexus -i '+ str(instrument) + ' ' + str(run_number)).read()
        self.fullfilenameInfo.setText(result)
     
        #check that nexus is a real file
        result = result.split('\n')
        _file = os.path.expanduser(result[0])
        if os.path.exists(_file):
            self.retrieve_metadata(_file)

    def retrieve_metadata(self,file):
        #FIXME
        #use your nexus parser to retrieve the various metadata

    def on_browseButton_clicked(self):
        print 'in on_browseButton_clicked'
        
if __name__ == "__main__":
    import sys
    
    app = QApplication(sys.argv)
    form = ViewNexDlg()
    form.show()
    app.exec_()
    
