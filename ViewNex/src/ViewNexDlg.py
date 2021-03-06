import re
from PyQt4.QtCore import *
from PyQt4.QtGui import *
import ui_ViewNexDlg

class ViewNexDlg(QDialog, ui_ViewNexDlg.Ui_ViewNexDlg):
    
    def __init__(self, parent=None):
        super(ViewNexDlg, self).__init__(parent)
        self.setupUi(self)

    def on_runLineEdit_textEdited(self, runnum):
        self.index = 0

    def runnum(self):
        return self.__runnum

    def instrumentsDropList_valueChanged(self, inst):
        return self.apply

    def on_searchButton_clicked(self):
        findnexus.exec_(i, runnum, inst)
        
if __name__ == "__main__":
    import sys
    
    app = QApplication(sys.argv)
    form = ViewNexDlg()
    form.show()
    app.exec_()
    
