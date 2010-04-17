import os
import sys
from PyQt4.QtCore import *
from PyQt4.QtGui import *

__version__ = "1.0.0"

class MainForm(QDialog):

    def __init__(self, parent=None):
        super(MainForm, self).__init__(parent)
        
        #left part of gui (widget tree)
        self.treeWidget = QTreeWidget()
        hLayout = QHBoxLayout()
        
        #right part (image, description and launch button)
        vLayout = QVBoxLayout()
        self.image = QLabel("here will go the preview of the application")
        self.description = QLabel("here is the description")
        self.launch = QPushButton("LAUNCH APPLICATION")
#        self.launch.setFixedHeight(30)
        vLayout.addWidget(self.image)
        vLayout.addWidget(self.description)
        vLayout.addWidget(self.launch)
        
        #layout left and right part together
        hLayout.addWidget(self.treeWidget)
        hLayout.addLayout(vLayout)

 #       self.infoSplitter = QSplitter(Qt.Vertical)
 #       self.infoSplitter.addWidget(self.image)
 #       self.infoSplitter.addWidget(self.description)
#        self.infoSplitter.addWidget(self.launch)
 #       self.mainSplitter = QSplitter(Qt.Horizontal)
 #       self.mainSplitter.addWidget(self.tree)
 #       self.mainSplitter.addWidget(self.infoSplitter)
        
        self.setLayout(hLayout)
        
#        self.setCentralWidget(self.mainSplitter)
        self.setWindowTitle("SNS applications tools launcher")
        self.setMinimumWidth(500)
        self.setMinimumHeight(300)
        QTimer.singleShot(0, self.initialLoad)

    def initialLoad(self):
        self.populateTree()

    def populateTree(self):
        selected = None
        self.treeWidget.clear()
        self.treeWidget.setColumnCount(1)
#        self.treeWidget.setHeaderLabels(["Type/Application"])
        self.treeWidget.setHeaderHidden(True)
        self.treeWidget.setItemsExpandable(True)
        
        ancestor = QTreeWidgetItem(self.treeWidget, ['Application'])
        self.treeWidget.expandItem(ancestor)
        parent = QTreeWidgetItem(ancestor, ['CLoopES'])
        parent = QTreeWidgetItem(ancestor, ['DAD'])
        parent = QTreeWidgetItem(ancestor, ['DGSreduction'])
        parent = QTreeWidgetItem(ancestor, ['FITStools'])
        parent = QTreeWidgetItem(ancestor, ['GG'])
        parent = QTreeWidgetItem(ancestor, ['MakeNeXus'])
        parent = QTreeWidgetItem(ancestor, ['plotARCS'])
        parent = QTreeWidgetItem(ancestor, ['plotASCII'])
#        self.treeWidget.setCurrentItem(parent)
        parent = QTreeWidgetItem(ancestor, ['plotBSS'])
        parent = QTreeWidgetItem(ancestor, ['plotCNCS'])
        parent = QTreeWidgetItem(ancestor, ['plotInstrument'])
        parent = QTreeWidgetItem(ancestor, ['REFoffSpec'])
        parent = QTreeWidgetItem(ancestor, ['REFreduction'])
        item = QTreeWidgetItem(parent, ['1.3.x versions'])
        item = QTreeWidgetItem(parent, ['1.4.x versions'])
        item = QTreeWidgetItem(parent, ['1.5.x versions'])                
        parent = QTreeWidgetItem(ancestor, ['REFscale'])        
        parent = QTreeWidgetItem(ancestor, ['SANSreduction'])
        
        ancestor = QTreeWidgetItem(self.treeWidget, ['Reduction'])
        parent = QTreeWidgetItem(ancestor, ['BSS'])
        parent = QTreeWidgetItem(ancestor, ['EQ-SANS'])
        parent = QTreeWidgetItem(ancestor, ['REF_L'])
        item = QTreeWidgetItem(parent, ['New rotated detector (304x256)'])
        item2 = QTreeWidgetItem(item, ['Last released version'])
        item2 = QTreeWidgetItem(item, ['Stable version'])        
        item = QTreeWidgetItem(parent, ['Old detector (256x304)'])
        item2 = QTreeWidgetItem(item, ['Last released version'])
        item2 = QTreeWidgetItem(item, ['Stable version'])        
        parent = QTreeWidgetItem(ancestor, ['REF_M'])
        item = QTreeWidgetItem(parent, ['New detector (128x128)'])
        item2 = QTreeWidgetItem(item, ['Last released version'])
        item2 = QTreeWidgetItem(item, ['Stable version'])        
        item = QTreeWidgetItem(parent, ['Old detector (304x256)'])
        item2 = QTreeWidgetItem(item, ['Last released version'])
        item2 = QTreeWidgetItem(item, ['Stable version'])        
        
        ancestor = QTreeWidgetItem(self.treeWidget, ['Visualization'])
        
        ancestor = QTreeWidgetItem(self.treeWidget, ['Utilities'])        
        
        ancestor = QTreeWidgetItem(self.treeWidget, ['Test'])

app = QApplication(sys.argv)
#    app.setApplicationName("Image Changer")
form = MainForm()
form.show()
app.exec_()


