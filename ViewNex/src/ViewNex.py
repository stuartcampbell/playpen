from  __future__ import division
import sys
from math import *
from PyQt4.QtCore import *
from PyQt4.QtGui import *
class Form(QDialog):
    def __init__(self, parent=None):
        super(Form, self).__init__(parent)
  
        self.run_number = QLineEdit("Run number")
        browse = QLineEdit("Browse Path")
        searchButton = QPushButton("Search")
        self.connect(searchButton, SIGNAL("clicked()"), self.launch_search)
        browseButton = QPushButton("Browse")
        protonchargeLabel = QLabel("<p ALIGN=RIGHT>" + "Proton Charge" + "</p>")
        timestartLabel = QLabel("<p ALIGN=RIGHT>" + "Time Start" + "</p>")
        durationLabel = QLabel("<p ALIGN=RIGHT>" + "Duration" + "</p>")
        slit1Label = QLabel("<p ALIGN=RIGHT>" + "Slit 1" + "</p>")
        slit2Label = QLabel("<p ALIGN=RIGHT>" + "Slit 2" + "</p>")
        slit3Label = QLabel("<p ALIGN=RIGHT>" + "Slit 3" + "</p>")
        slit4Label = QLabel("<p ALIGN=RIGHT>" + "Slit 4" + "</p>")
        tthetaLabel = QLabel("<p ALIGN=RIGHT>" + "Ttheta" + "</p>")
        tthdLabel = QLabel("<p ALIGN=RIGHT>" + "TTHD" + "</p>")
        thsLabel = QLabel("<p ALIGN=RIGHT>" + "THS" + "</p>")
        thiLabel = QLabel("<p ALIGN=RIGHT>" + "THI" + "</p>")
        totalcountsLabel = QLabel("<p ALIGN=RIGHT>" + "Total Counts" + "</p>")
        ccwLabel = QLabel("<p ALIGN=RIGHT>" + "Chopper Center Wavelength" + "</p>")
        self.protonchargeInfo = QLineEdit("")
        timestartInfo = QLineEdit("")
        durationInfo = QLineEdit("")
        slit1Info = QLineEdit("")
        slit2Info = QLineEdit("")
        slit3Info = QLineEdit("")
        slit4Info = QLineEdit("")
        tthetaInfo = QLineEdit("")
        tthdInfo = QLineEdit("")
        thsInfo = QLineEdit("")
        thiInfo = QLineEdit("")
        totalcountsInfo = QLineEdit("")
        ccwInfo = QLineEdit("")
        layout = QGridLayout()
        layout.addWidget(self.run_number, 0,0,1,2)
        layout.addWidget(browse, 1,0,1,2)
        layout.addWidget(searchButton, 0,2)
        layout.addWidget(browseButton, 1,2)
        layout.addWidget(protonchargeLabel, 2,0)
        layout.addWidget(timestartLabel, 3,0)
        layout.addWidget(durationLabel, 4,0)
        layout.addWidget(slit1Label, 5,0)
        layout.addWidget(slit2Label, 6,0)
        layout.addWidget(slit3Label, 7,0)
        layout.addWidget(slit4Label, 8,0)
        layout.addWidget(tthetaLabel, 9,0)
        layout.addWidget(tthdLabel, 10,0)
        layout.addWidget(thsLabel, 11,0)
        layout.addWidget(thiLabel, 12,0)
        layout.addWidget(totalcountsLabel, 13,0)
        layout.addWidget(ccwLabel, 14,0)
        layout.addWidget(self.protonchargeInfo, 2,1)
        layout.addWidget(timestartInfo, 3,1)
        layout.addWidget(durationInfo, 4,1)
        layout.addWidget(slit1Info, 5,1)
        layout.addWidget(slit2Info, 6,1)
        layout.addWidget(slit3Info, 7,1)
        layout.addWidget(slit4Info, 8,1)
        layout.addWidget(tthetaInfo, 9,1)
        layout.addWidget(tthdInfo, 10,1)
        layout.addWidget(thsInfo, 11,1)
        layout.addWidget(thiInfo, 12,1)
        layout.addWidget(totalcountsInfo, 13,1)
        layout.addWidget(ccwInfo, 14,1)
        self.setLayout(layout)
        self.run_number.setFocus()
        self.setWindowTitle("ViewNex")
        
    def launch_search(self):
        #retrieve run number entered in the first text box
        run_number = self.run_number.text()
        #run findnexus command with run number retrieved from the run number box
        #command to run in the back
        #  > findnexus -i <instrument> run_number
        # retrieve what findnexus will return (should be the full path to a nexus file)
        # then use that file name with your readnxs.py function to retrieve the various fields we want
        # let suppose you retrieve the proton charge, to put this value in the right box
        self.protonchargeInfo.setText("1.4545454")
        
        
        
app = QApplication(sys.argv)
form = Form()
form.show()
app.exec_()
