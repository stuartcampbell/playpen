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
        self.exe = {}

        #left part of gui (widget tree)
        self.treeWidget = QTreeWidget()
        hLayout = QHBoxLayout()
        
        #right part (image, description and launch button)
        vLayout = QVBoxLayout()
#        self.image = QLabel("here will go the preview of the application")
        #pixmap = QPixmap(":/under_construction.gif")
        self.image = QLabel(self)
        #self.image.setPixmap(pixmap)
        
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

        self.connect(self.treeWidget, SIGNAL("itemClicked(QTreeWidgetItem*,int)"), self.tree_click_event)
        self.connect(self.treeWidget, SIGNAL("itemSelectionChanged()"), self.tree_select_event)
        self.connect(self.launch, SIGNAL("clicked()"), self.launch_application)
        self.connect(self.help, SIGNAL("clicked()"), self.launch_help)

#        self.setCentralWidget(self.mainSplitter)
        self.setWindowTitle("SNS applications launcher")
        self.setMinimumWidth(1000)
        self.setMinimumHeight(500)
        
        QTimer.singleShot(0, self.initialLoad)

    def launch_help(self):
        os.system('/SNS/software/idltools/NeedHelp &')

    def launch_application(self):
        print 'launching current selected application'
#        item = self.treeWidget.item

    def tree_select_event(self):
        item = self.treeWidget.selectedItems()
        appli = self.itemDict[item[0]]
        self.descriptionWidget.setText(self.description[appli])
        pixmap = QPixmap(":/%s.gif" % self.screen[appli])
        self.image.setPixmap(pixmap)
        if self.exe[appli] is None:
            self.launch.hide()
        else:
            self.launch.show()

    def tree_click_event(self, item):
        appli = self.itemDict[item]
        self.descriptionWidget.setText(self.description[appli])
        pixmap = QPixmap(":/%s.gif" % self.screen[appli])
        self.image.setPixmap(pixmap)
        if self.exe[appli] is None:
            self.launch.hide()
        else:
            self.launch.show()

    def initialLoad(self):
        self.populateTree()
        
    def createItem(self, ancestor, itemName, label=None, exeName=None, imageName=None):
        if label is None:
            local_label = itemName
        else:
            local_label = label
        parent = QTreeWidgetItem(ancestor, [local_label])
        self.itemDict[parent] = itemName
        if itemName not in self.description.keys():
            self.description[itemName] = "Description of " + itemName
            self.screen[itemName] = imageName
            self.exe[itemName] = exeName
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
        parent = self.createItem(ancestor, 'BSSreduction', exeName='BSSreduction', imageName='BSSreduction')        
        parent = self.createItem(ancestor, 'CLoopES', exeName='CLoopES', imageName='CLoopES')
        parent = self.createItem(ancestor, 'DAD', imageName='DAD')
        parent = self.createItem(ancestor, 'DGSreduction', imageName='DGSreduction')
        parent = self.createItem(ancestor, 'FITStools', imageName='FITStools')
        parent = self.createItem(ancestor, 'Geometry Generator', imageName='GG')
        parent = self.createItem(ancestor, 'MakeNeXus', imageName='MakeNeXus')
        parent = self.createItem(ancestor, 'plotARCS', imageName='plotARCS')
        parent = self.createItem(ancestor, 'plotASCII', imageName='plotASCII')
        parent = self.createItem(ancestor, 'plotBSS', imageName='plotBSS')
        parent = self.createItem(ancestor, 'plotCNCS', imageName='plotCNCS')
        parent = self.createItem(ancestor, 'plotInstrument', imageName='under_construction')
        parent = self.createItem(ancestor, 'plotROI', imageName='plotROI')
        parent = self.createItem(ancestor, 'realignBSS', exeName='realignBSS', imageName='realignBSS')
        parent = self.createItem(ancestor, 'REFoffSpec', imageName='REFoffSpec')
        parent = self.createItem(ancestor, 'REFreduction')
        item = self.createItem(parent, '1.3.x versions')
        item2 = self.createItem(item, '1.3.x high resolution version',
                                label="High resolution version",
                                imageName='REFreduction1_3')
        item2 = self.createItem(item, '1.3.x low resolution version',
                                label='Low resolution version',
                                imageName='miniREFreduction1_3')
        item = self.createItem(parent, '1.5.x versions')
        item2 = self.createItem(item, '1.5.x high resolution version',
                                label='High resolution version',
                                imageName='REFreduction1_5')
        item2 = self.createItem(item, '1.5.x low resolution version',
                                label='Low resolution version',
                                imageName='miniREFreduction1_5')
        item = self.createItem(parent, '1.6.x versions')
        item2 = self.createItem(item, '1.6.x high resolution version',
                                label='High resolution version',
                                imageName='REFreduction1_6')
        item2 = self.createItem(item, '1.6.x low resolution version',
                                label='Low resolution version',
                                imageName='miniREFreduction1_6')
        parent = self.createItem(ancestor, 'REFscale', imageName='REFscale')
        parent = self.createItem(ancestor, 'SANSreduction')
        item = self.createItem(parent, 'SANSreduction high resolution version',
                               label='High resolution version',
                               imageName='SANSreduction')
        item = self.createItem(parent, 'SANSreduction low resolution version',
                               label='Low resolution version',
                               imageName='miniSANSreduction')
        
        ##Instrument
        ancestor = self.createItem(self.treeWidget, 'Instruments')
        #ARCS
        parent = self.createItem(ancestor, 'ARCS')        
        item = self.createItem(parent, 'DGSreduction')        
        item = self.createItem(parent, 'Geometry Generator')
        item = self.createItem(parent, 'MakeNeXus')
        item = self.createItem(parent, 'plotARCS')
        item = self.createItem(parent, 'plotROI')
        #BSS
        parent = self.createItem(ancestor, 'BSS')        
        item = self.createItem(parent, 'BSSreduction')
        item = self.createItem(parent, 'CLoopES')
        item = self.createItem(parent, 'DAD')        
        item = self.createItem(parent, 'Geometry Generator')
        item = self.createItem(parent, 'MakeNeXus')
        item = self.createItem(parent, 'plotBSS')
        item = self.createItem(parent, 'plotROI')        
        item = self.createItem(parent, 'realignBSS')
        #EQSANS
        parent = self.createItem(ancestor, 'EQSANS')        
        item = self.createItem(parent, 'Geometry Generator')
        item = self.createItem(parent, 'MakeNeXus')
        item = self.createItem(parent, 'plotROI')
        item = self.createItem(parent, 'SANSreduction') 
        item2 = self.createItem(item, 'SANSreduction high resolution version',
                                label='High resolution version')
        item2 = self.createItem(item, 'SANSreduction low resolution version',
                                label='Low resolution version')                               
        #CNCS
        parent = self.createItem(ancestor, 'CNCS')        
        item = self.createItem(parent, 'DGSreduction')
        item = self.createItem(parent, 'Geometry Generator')
        item = self.createItem(parent, 'MakeNeXus')
        item = self.createItem(parent, 'plotCNCS')
        item = self.createItem(parent, 'plotROI')        
        #REF_L
        parent = self.createItem(ancestor, 'REF_L')
        item = self.createItem(parent, 'Geometry Generator')
        item = self.createItem(parent, 'MakeNeXus')
        item = self.createItem(parent, 'plotASCII')
        item = self.createItem(parent, 'plotROI')
        item = self.createItem(parent, 'REFoffSpec')
        item = self.createItem(parent, 'REFreduction')
        item2 = self.createItem(item, '1.3.x versions')
        item3 = self.createItem(item2, '1.3.x high resolution version',
                                label='High resolution version')
        item3 = self.createItem(item2, '1.3.x low resolution version',
                                label='Low resolution version')
        item2 = self.createItem(item, '1.5.x versions')
        item3 = self.createItem(item2, '1.5.x high resolution version',
                                label='High resolution version')
        item3 = self.createItem(item2, '1.5.x low resolution version',
                                label='Low resolution version')
        parent = self.createItem(parent, 'REFscale')
        #REF_M
        parent = self.createItem(ancestor, 'REF_M')
        item = self.createItem(parent, 'Geometry Generator')
        item = self.createItem(parent, 'MakeNeXus')
        item = self.createItem(parent, 'plotASCII')
        item = self.createItem(parent, 'plotROI')
        item = self.createItem(parent, 'REFoffSpec')
        item = self.createItem(parent, 'REFreduction')
        item2 = self.createItem(item, '1.3.x versions')
        item3 = self.createItem(item2, 'High resolution version',
                                label='1.3.x high resolution version')
        item3 = self.createItem(item2, 'Low resolution version',
                                label='1.3.x low resolution version')
        item2 = self.createItem(item, '1.6.x versions')
        item3 = self.createItem(item2, '1.6.x high resolution version',
                                label='High resolution version')
        item3 = self.createItem(item2, '1.6.x low resolution version',
                                label='Low resolution version')
        parent = self.createItem(parent, 'REFscale')
        #SEQUOIA
        parent = self.createItem(ancestor, 'SEQUOIA')
        item = self.createItem(parent, 'DGSreduction')
        item = self.createItem(parent, 'Geometry Generator')
        item = self.createItem(parent, 'MakeNeXus')
        item = self.createItem(parent, 'plotASCII')
        item = self.createItem(parent, 'plotROI')
        #VENUS
        parent = self.createItem(ancestor, 'VENUS')        
        item = self.createItem(parent, 'FITStools')        
        
        ##Reduction
        ancestor = self.createItem(self.treeWidget, 'Reduction')
        parent = self.createItem(ancestor, "reduction ARCS", 
                                label = 'ARCS',
                                imageName='DGSreduction')
        parent = self.createItem(ancestor, "reduction BSS", 
                                 label = 'BSS',
                                 imageName='BSSreduction')        
        parent = self.createItem(ancestor, "reduction CNCS", 
                                 label = 'CNCS',
                                 imageName='DGSreduction')
        parent = self.createItem(ancestor, "reduction EQSANS",
                                 label = 'EQSANS')
        item = self.createItem(parent, 'SANSreduction high resolution version',
                               label = 'High resolution version')
        item = self.createItem(parent, 'SANSreduction low resolution version',
                               label = 'Low resolution version')
        parent = self.createItem(ancestor, "reduction REF_L",
                                 label = 'REF_L')
        item = self.createItem(parent, 'REFoffSpec')
        item = self.createItem(parent, 'REFreduction')

        item2 = self.createItem(item, '1.3.x versions (old detector 256x304)')
        item3 = self.createItem(item2, '1.3.x high resolution version',
                                label = 'High resolution version')
        item4 = self.createItem(item3, '1.3.x low resolution version', 
                                label = 'Low resolution version',
                                imageName='miniREFreduction1_3')
        item2 = self.createItem(item, '1.5.x versions (new detector 304x256)')
        item3 = self.createItem(item2, '1.5.x high resolution version',
                                label = 'High resolution version')
        item4 = self.createItem(item3, '1.5.x low resolution version', 
                                label = 'Low resolution version',
                                imageName='miniREFreduction1_3')

        parent = self.createItem(ancestor, "reduction REF_M",
                                 label = 'REF_M')
        item = self.createItem(parent, 'REFoffSpec')
        item = self.createItem(parent, 'REFreduction')

        item2 = self.createItem(item, '1.3.x versions (old detector 304x256)')
        item3 = self.createItem(item2, '1.3.x high resolution version',
                                label = 'High resolution version')
        item4 = self.createItem(item3, '1.3.x low resolution version', 
                                label = 'Low resolution version',
                                imageName='miniREFreduction1_3')
        item2 = self.createItem(item, '1.6.x versions (new detector 304x256)')
        item3 = self.createItem(item2, '1.6.x high resolution version',
                                label = 'High resolution version')
        item4 = self.createItem(item3, '1.6.x low resolution version', 
                                label = 'Low resolution version',
                                imageName='miniREFreduction1_3')

        parent = self.createItem(ancestor, "reduction SEQUOIA",
                                 label = 'SEQUOIA',
                                 imageName = 'DGSreduction')
                
        ##Utilities        
        ancestor = self.createItem(self.treeWidget, 'Utilities')        
        parent = self.createItem(ancestor, 'CLoopES')
        parent = self.createItem(ancestor, 'DAD')
        parent = self.createItem(ancestor, 'FITStools')
        parent = self.createItem(ancestor, 'Geometry Generator')
        parent = self.createItem(ancestor, 'MakeNeXus')

        ##Visualization
        ancestor = self.createItem(self.treeWidget, 'Visualization')
        parent = self.createItem(ancestor, "plotARCS")
        parent = self.createItem(ancestor, "plotASCII")                
        parent = self.createItem(ancestor, "plotBSS")
        parent = self.createItem(ancestor, "plotCNCS")
        parent = self.createItem(ancestor, "plotInstruments", imageName='under_construction')
        parent = self.createItem(ancestor, "plotROI")

        ##Test
        ancestor = self.createItem(self.treeWidget, 'Test')
        parent = self.createItem(ancestor, "")

app = QApplication(sys.argv)
#    app.setApplicationName("Image Changer")
form = MainForm()
form.show()
app.exec_()


