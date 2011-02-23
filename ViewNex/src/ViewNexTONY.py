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
        self.protonchargeInfo.setText("1.4545454")
        os.system(findnexus -i, inst, runnum)
        findnexus.exec_(i, runnum, inst)

    def on_browseButton_clicked(self):
        self.protonchargeInfo.setText("1.4545454")
        
if __name__ == "__main__":
    import sys
    
    app = QApplication(sys.argv)
    form = ViewNexDlg()
    form.show()
    app.exec_()
    
