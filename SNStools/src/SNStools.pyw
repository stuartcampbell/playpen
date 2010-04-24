import os
import sys
from PyQt4.QtCore import *
from PyQt4.QtGui import *
import qrc_resources

__version__ = "1.0.0"

class MainForm(QDialog):

    def __init__(self, parent=None):
        super(MainForm, self).__init__(parent)
        
        self.itemDict = {}
        self.description = {}
        self.screen = {}

        #left part of gui (widget tree)
        self.treeWidget = QTreeWidget()
        hLayout = QHBoxLayout()
        
        #right part (image, description and launch button)
        vLayout = QVBoxLayout()
#        self.image = QLabel("here will go the preview of the application")
        pixmap = QPixmap(":/under_construction.gif")
        self.image = QLabel(self)
        self.image.setPixmap(pixmap)
        
        self.descriptionWidget = QLabel("here is the description")
        
        self.launch = QPushButton("LAUNCH APPLICATION")
        self.help = QPushButton("HELP")
        h2Layout = QHBoxLayout()
        h2Layout.addWidget(self.launch)
        h2Layout.addWidget(self.help)
        
        vLayout.addWidget(self.image)
        vLayout.addWidget(self.descriptionWidget)
        vLayout.addLayout(h2Layout)
        
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

        self.connect(self.treeWidget, SIGNAL("itemClicked(QTreeWidgetItem*,int)"), self.tree_event)
        self.connect(self.launch, SIGNAL("clicked()"), self.launch_application)
        self.connect(self.help, SIGNAL("clicked()"), self.launch_help)

#        self.setCentralWidget(self.mainSplitter)
        self.setWindowTitle("SNS applications launcher")
        self.setMinimumWidth(500)
        self.setMinimumHeight(300)
        
        QTimer.singleShot(0, self.initialLoad)

    def launch_help(self):
        os.system('/SNS/software/idltools/NeedHelp &')

    def launch_application(self):
        print 'launching current selected application'
#        item = self.treeWidget.item

    def tree_event(self, item):
        appli = self.itemDict[item]
        self.descriptionWidget.setText(self.description[appli])
        pixmap = QPixmap(":/%s.gif" % self.screen[appli])
        self.image.setPixmap(pixmap)

    def initialLoad(self):
        self.populateTree()
        
    def createItem(self, ancestor, itemName):
        parent = QTreeWidgetItem(ancestor, [itemName])
        self.itemDict[parent] = itemName
        self.description[itemName] = "Description of " + itemName
        self.screen[itemName] = itemName
        return parent
        
    def populateTree(self):
        selected = None
        self.treeWidget.clear()
        self.treeWidget.setColumnCount(1)
#        self.treeWidget.setHeaderLabels(["Type/Application"])
        self.treeWidget.setHeaderHidden(True)
        self.treeWidget.setItemsExpandable(True)
        
        ##Application
        ancestor = self.createItem(self.treeWidget, 'Application')
        self.treeWidget.expandItem(ancestor)
        parent = self.createItem(ancestor, 'CLoopES')
        parent = self.createItem(ancestor, 'DAD')
        parent = self.createItem(ancestor, 'DGSreduction')
        parent = self.createItem(ancestor, 'FITStools')
        parent = self.createItem(ancestor, 'GG')
        parent = self.createItem(ancestor,'MakeNeXus')
        parent = self.createItem(ancestor,'plotARCS')
        parent = self.createItem(ancestor,'plotASCII')
        parent = self.createItem(ancestor,'plotBSS')
        parent = self.createItem(ancestor,'plotCNCS')
        parent = self.createItem(ancestor,'plotInstrument')
        parent = self.createItem(ancestor,'plotROI')
        parent = self.createItem(ancestor,'REFoffSpec')
        parent = self.createItem(ancestor,'REFreduction')
        item = self.createItem(parent,'1.3.x versions')
        item2 = self.createItem(item, 'High resolution version')
        item2 = self.createItem(item, 'Low resolution version')
        item = self.createItem(parent,'1.4.x versions')
        item2 = self.createItem(item, 'High resolution version')
        item2 = self.createItem(item, 'Low resolution version')
        item = self.createItem(parent,'1.5.x versions')
        item2 = self.createItem(item, 'High resolution version')
        item2 = self.createItem(item, 'Low resolution version')
        parent = self.createItem(ancestor,'REFscale')
        parent = self.createItem(ancestor,'SANSreduction')
        item = self.createItem(parent, 'High resolution version')
        item = self.createItem(parent, 'Low resolution version')
        
        ##Instrument
        ancestor = self.createItem(self.treeWidget, 'Instruments')
        #ARCS
        parent = self.createItem(ancestor,'ARCS')        
        item = self.createItem(parent, 'Geometry Generator')
        item = self.createItem(parent, 'MakeNeXus')
        item = self.createItem(parent, 'plotARCS')
        item = self.createItem(parent, 'plotROI')
        #BSS
        parent = self.createItem(ancestor,'BSS')        
        item = self.createItem(parent, 'BSSreduction')
        item = self.createItem(parent, 'CLoopES')
        item = self.createItem(parent, 'DAD')        
        item = self.createItem(parent, 'Geometry Generator')
        item = self.createItem(parent, 'MakeNeXus')
        item = self.createItem(parent, 'plotBSS')
        item = self.createItem(parent, 'plotROI')        
        item = self.createItem(parent, 'realignBSS')
        #EQSANS
        parent = self.createItem(ancestor,'EQSANS')        
        item = self.createItem(parent, 'Geometry Generator')
        item = self.createItem(parent, 'MakeNeXus')
        item = self.createItem(parent, 'plotROI')
        item = self.createItem(parent, 'SANSreduction')                        
        #CNCS
        parent = self.createItem(ancestor,'CNCS')        
        item = self.createItem(parent, 'DGSreduction')
        item = self.createItem(parent, 'Geometry Generator')
        item = self.createItem(parent, 'MakeNeXus')
        item = self.createItem(parent, 'plotCNCS')
        item = self.createItem(parent, 'plotROI')        
        #REF_L
        parent = self.createItem(ancestor,'REF_L')
        item = self.createItem(parent, 'Geometry Generator')
        item = self.createItem(parent, 'MakeNeXus')
        item = self.createItem(parent, 'plotASCII')
        item = self.createItem(parent, 'plotROI')
        item = self.createItem(parent,'REFoffSpec')
        item = self.createItem(parent,'REFreduction')
        item2 = self.createItem(item,'1.3.x versions')
        item3 = self.createItem(item2, 'High resolution version')
        item3 = self.createItem(item2, 'Low resolution version')
        item2 = self.createItem(item,'1.5.x versions')
        item3 = self.createItem(item2, 'High resolution version')
        item3 = self.createItem(item2, 'Low resolution version')
        parent = self.createItem(parent,'REFscale')
        #REF_M
        parent = self.createItem(ancestor,'REF_M')
        item = self.createItem(parent, 'Geometry Generator')
        item = self.createItem(parent, 'MakeNeXus')
        item = self.createItem(parent, 'plotASCII')
        item = self.createItem(parent, 'plotROI')
        item = self.createItem(parent,'REFoffSpec')
        item = self.createItem(parent,'REFreduction')
        item2 = self.createItem(item,'1.3.x versions')
        item3 = self.createItem(item2, 'High resolution version')
        item3 = self.createItem(item2, 'Low resolution version')
        item2 = self.createItem(item,'1.6.x versions')
        item3 = self.createItem(item2, 'High resolution version')
        item3 = self.createItem(item2, 'Low resolution version')
        parent = self.createItem(parent,'REFscale')
        #SEQUOIA
        parent = self.createItem(ancestor,'SEQUOIA')
        item = self.createItem(parent, 'Geometry Generator')
        item = self.createItem(parent, 'MakeNeXus')
        item = self.createItem(parent, 'plotASCII')
        item = self.createItem(parent, 'plotROI')
        item = self.createItem(parent, 'DGSreduction')
        #VENUS
        parent = self.createItem(ancestor,'VENUS')        
        item = self.createItem(parent, 'FITStools')        
        
        ##Reduction
        ancestor = self.createItem(self.treeWidget, 'Reduction')
        parent = self.createItem(ancestor, "ARCS")
        parent = self.createItem(ancestor, "BSS")        
        parent = self.createItem(ancestor, "CNCS")
        parent = self.createItem(ancestor, "EQSANS")
        item = self.createItem(parent, 'High resolution version')
        item = self.createItem(parent, 'Low resolution version')        
        parent = self.createItem(ancestor, "REF_L")
        item = self.createItem(parent,'REFoffSpec')
        item = self.createItem(parent,'REFreduction')
        item2 = self.createItem(item,'1.3.x versions (old detector 256x304)')
        item3 = self.createItem(item2, 'High resolution version')
        item4 = self.createItem(item3, 'Stable version')
        item4 = self.createItem(item3, 'Last released version')        
        item3 = self.createItem(item2, 'Low resolution version')
        item4 = self.createItem(item3, 'Stable version')
        item4 = self.createItem(item3, 'Last released version')        
        item2 = self.createItem(item,'1.5.x versions (new detector 304x256)')
        item3 = self.createItem(item2, 'High resolution version')
        item4 = self.createItem(item3, 'Stable version')
        item4 = self.createItem(item3, 'Last released version')        
        item3 = self.createItem(item2, 'Low resolution version')
        item4 = self.createItem(item3, 'Stable version')
        item4 = self.createItem(item3, 'Last released version')        
        parent = self.createItem(ancestor, "REF_M")
        item = self.createItem(parent,'REFoffSpec')
        item = self.createItem(parent,'REFreduction')
        item2 = self.createItem(item,'1.3.x versions (old detector 304x256)')
        item3 = self.createItem(item2, 'High resolution version')
        item4 = self.createItem(item3, 'Stable version')
        item4 = self.createItem(item3, 'Last released version')        
        item3 = self.createItem(item2, 'Low resolution version')
        item4 = self.createItem(item3, 'Stable version')
        item4 = self.createItem(item3, 'Last released version')        
        item2 = self.createItem(item,'1.6.x versions (new detector 128x128)')
        item3 = self.createItem(item2, 'High resolution version')
        item4 = self.createItem(item3, 'Stable version')
        item4 = self.createItem(item3, 'Last released version')        
        item3 = self.createItem(item2, 'Low resolution version')
        item4 = self.createItem(item3, 'Stable version')
        item4 = self.createItem(item3, 'Last released version')        
        parent = self.createItem(ancestor, "SEQUOIA")
                
        ##Utilities        
        ancestor = self.createItem(self.treeWidget, 'Utilities')        
        parent = self.createItem(ancestor,'CLoopES')
        parent = self.createItem(ancestor,'DAD')
        parent = self.createItem(ancestor,'FITStools')
        parent = self.createItem(ancestor,'Geometry Generator')
        parent = self.createItem(ancestor,'MakeNeXus')

        ##Visualization
        ancestor = self.createItem(self.treeWidget, 'Utilities')
        parent = self.createItem(ancestor, "plotARCS")
        parent = self.createItem(ancestor, "plotASCII")                
        parent = self.createItem(ancestor, "plotBSS")
        parent = self.createItem(ancestor, "plotCNCS")
        parent = self.createItem(ancestor, "plotInstruments")
        parent = self.createItem(ancestor, "plotROI")

app = QApplication(sys.argv)
#    app.setApplicationName("Image Changer")
form = MainForm()
form.show()
app.exec_()


