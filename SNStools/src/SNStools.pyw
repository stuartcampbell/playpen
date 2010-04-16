import os
import sys
from PyQt4.QtCore import *
from PyQt4.QtGui import *

__version__ = "1.0.0"

class MainWindow(QMainWindow):

    def __init__(self, parent=None):
        super(MainWindow, self).__init__(parent)
        
        #left part of gui (widget tree)
        self.tree = QTreeWidget()
        hLayout = QHBoxLayout()
        
        #right part (image, description and launch button)
        vLayout = QVBoxLayout()
        self.image = QLabel("here will go the preview of the application")
        self.description = QLabel("here is the description")
        self.launch = QPushButton("LAUNCH APPLICATION")
        self.launch.setFixedHeight(30)
        vLayout.addWidget(self.image)
        vLayout.addWidget(self.description)
        vLayout.addWidget(self.launch)
        
        #layout left and right part together
        hLayout.addWidget(self.tree)
        hLayout.addLayout(vLayout)

        self.infoSplitter = QSplitter(Qt.Vertical)
        self.infoSplitter.addWidget(self.image)
        self.infoSplitter.addWidget(self.description)
#        self.infoSplitter.addWidget(self.launch)
        self.mainSplitter = QSplitter(Qt.Horizontal)
        self.mainSplitter.addWidget(self.tree)
        self.mainSplitter.addWidget(self.infoSplitter)
        

        self.setCentralWidget(self.mainSplitter)
        self.setWindowTitle("SNS applications tools launcher")
        self.setMinimumWidth(300)


app = QApplication(sys.argv)
#    app.setApplicationName("Image Changer")
form = MainWindow()
form.show()
app.exec_()


