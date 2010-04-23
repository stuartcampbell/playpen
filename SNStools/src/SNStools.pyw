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
        parent = QTreeWidgetItem(ancestor, ['REF_L'])
        self.itemDict[parent] = 'REF_L'        
        self.description['REF_L'] = "Description of REF_L"
        item = QTreeWidgetItem(parent, ['Geometry generator']) #GG
        self.itemDict[item] = 'Geometry generator'
        self.description['Geometry generator'] = "Description of geometry generator"        
        item = QTreeWidgetItem(parent, ['MakeNeXus']) #MakeNeXus
        self.itemDict[item] = 'MakeNeXus'
        self.description['MakeNeXus'] = "Description of MakeNeXus" 
        item = QTreeWidgetItem(parent, ['plotROI']) #plotROI
        self.itemDict[item] = 'plotROI'
        self.description['plotROI'] = "Description of plotROI"        
        item = QTreeWidgetItem(parent, ['REFoffSpec']) #REFoffSpec
        self.itemDict[item] = 'REFoffSpec'        
        self.description['REFoffSpec'] = "Description of REFoffSpec"
        item = QTreeWidgetItem(parent, ['REFreduction']) #REFreduction
        self.itemDict[item] = 'REFreduction'        
        self.description['REFreduction'] = "Description of REFreduction"
        item2 = QTreeWidgetItem(item, ['1.3.x versions']) #1.3.x
        self.itemDict[item2] = '1.3.x versions'        
        self.description['1.3.x versions'] = "Description of 1.3.x versions"
        item3 = QTreeWidgetItem(item2, ['High resolution version']) #high resolution
        self.itemDict[item3] = 'High resolution version'
        self.description['High resolution version'] = "Description of high resolution version"
        item3 = QTreeWidgetItem(item2, ['Low resolution version']) #low resolution
        self.itemDict[item3] = 'Low resolution version'
        self.description['Low resolution version'] = "Description of low resolution version"
        item2 = QTreeWidgetItem(item, ['1.4.x versions']) #1.4.x
        self.itemDict[item2] = '1.4.x versions'
        self.description['1.4.x versions'] = "Description of 1.4.x versions"                
        item3 = QTreeWidgetItem(item2, ['High resolution version']) #high resolution
        self.itemDict[item3] = 'High resolution version'
        self.description['High resolution version'] = "Description of high resolution version"
        item3 = QTreeWidgetItem(item2, ['Low resolution version']) #low resolution
        self.itemDict[item3] = 'Low resolution version'
        self.description['Low resolution version'] = "Description of low resolution version"
        item = QTreeWidgetItem(parent, ['REFscale']) #REFscale        
        self.itemDict[item] = 'REFscale'
        self.description['REFscale'] = "Description of REFscale"                
        #REF_M
        parent = QTreeWidgetItem(ancestor, ['REF_M'])
        self.itemDict[parent] = 'REF_M'        
        self.description['REF_M'] = "Description of REF_M"
        item = QTreeWidgetItem(parent, ['Geometry generator']) #GG
        self.itemDict[item] = 'Geometry generator'
        self.description['Geometry generator'] = "Description of geometry generator"
        item = QTreeWidgetItem(parent, ['MakeNeXus']) #MakeNeXus
        self.itemDict[item] = 'MakeNeXus'
        self.description['MakeNeXus'] = "Description of MakeNeXus"                
        item = QTreeWidgetItem(parent, ['ASCII files']) #plotASCII
        self.itemDict[item] = 'ASCII files'   
        self.description[''] = "Description of "      
        item = QTreeWidgetItem(parent, ['plotROI']) #plotROI
        self.itemDict[item] = 'plotROI'
        self.description['plotROI'] = "Description of plotROI"        
        item = QTreeWidgetItem(parent, ['REFreduction']) #REFreduction
        self.itemDict[item] = 'REFreduction'        
        self.description['REFreduction'] = "Description of REFreduction"
        item2 = QTreeWidgetItem(item, ['1.3.x versions']) #1.3.x
        self.itemDict[item2] = '1.3.x versions'        
        self.description['1.3.x versions'] = "Description of 1.3.x versions"
        item3 = QTreeWidgetItem(item2, ['High resolution version']) #High resolution
        self.itemDict[item3] = 'High resolution version'
        self.description['High resolution version'] = "Description of high resolution version"
        item3 = QTreeWidgetItem(item2, ['Low resolution version']) #Low resolution
        self.itemDict[item3] = 'Low resolution version'
        self.description['Low resolution version'] = "Description of low resolution version"
        item2 = QTreeWidgetItem(item, ['1.5.x versions']) #1.5.x                
        self.itemDict[item2] = '1.5.x versions'        
        self.description['1.5.x versions'] = "Description of 1.5.x versions"
        item3 = QTreeWidgetItem(item2, ['High resolution version']) #High resolution
        self.itemDict[item3] = 'High resolution version'
        self.description['High resolution version'] = "Description of high resolution version"
        item3 = QTreeWidgetItem(item2, ['Low resolution version']) #Low resolution
        self.itemDict[item3] = 'Low resolution version'
        self.description['Low resolution version'] = "Description of low resolution version"
        item = QTreeWidgetItem(parent, ['REFscale']) #REFscale
        self.itemDict[item] = 'REFscale'
        self.description['REFscale'] = "Description of REFscale"                
        #SEQUOIA
        parent = QTreeWidgetItem(ancestor, ['SEQUOIA'])
        self.itemDict[parent] = 'SEQUOIA'        
        self.description['SEQUOIA'] = "Description of SEQUOIA"
        item = QTreeWidgetItem(parent, ['Geometry generator']) #GG
        self.itemDict[item] = 'Geometry generator'
        self.description['Geometry generator'] = "Description of geometry generator"
        item = QTreeWidgetItem(parent, ['MakeNeXus']) #MakeNeXus
        self.itemDict[item] = 'MakeNeXus'
        self.description['MakeNeXus'] = "Description of MakeNeXus"                
        item = QTreeWidgetItem(parent, ['ASCII files']) #plotASCII
        self.itemDict[item] = 'ASCII files'   
        self.description[''] = "Description of "        
        item = QTreeWidgetItem(parent, ['plotROI']) #plotROI
        self.itemDict[item] = 'plotROI'
        self.description['plotROI'] = "Description of plotROI"        
        item = QTreeWidgetItem(parent, ['DGSreduction']) #DGSreduction
        self.itemDict[item] = 'DGSreduction'        
        self.description['DGSreduction'] = "Description of DGSreduction"

        ##Reduction
        ancestor = QTreeWidgetItem(self.treeWidget, ['Reduction'])
        self.itemDict[ancestor] = 'Reduction'        
        self.description[''] = "Description of "
        ##ARCS
        parent = QTreeWidgetItem(ancestor, ['ARCS'])
        self.itemDict[parent] = 'ARCS'        
        self.description['ARCS'] = "Description of DGSreduction"        
        ##BSS
        parent = QTreeWidgetItem(ancestor, ['BSS'])
        self.itemDict[parent] = 'BSS'        
        self.description[''] = "Description of "
        ##CNCS
        parent = QTreeWidgetItem(ancestor, ['CNCS'])
        self.itemDict[parent] = 'CNCS'        
        self.description['CNCS'] = "Description of DGSreduction"        
        ##EQSANS
        parent = QTreeWidgetItem(ancestor, ['EQ-SANS'])
        self.itemDict[parent] = 'EQ-SANS'       
        self.description[''] = "Description of "         
        item = QTreeWidgetItem(parent, ['High resolution version'])
        self.itemDict[item] = 'High resolution version'
        self.description['High resolution version'] = "Description of high resolution version"
        item = QTreeWidgetItem(parent, ['Low resolution version'])
        self.itemDict[item] = 'Low resolution version'
        self.description['Low resolution version'] = "Description of low resolution version"
        ##REF_L
        parent = QTreeWidgetItem(ancestor, ['REF_L'])
        self.itemDict[parent] = 'REF_L'        
        self.description[''] = "Description of "        
        item = QTreeWidgetItem(parent, ['New rotated detector (304x256)'])
        self.itemDict[item] = 'New rotated detector (304x256)'        
        self.description[''] = "Description of "
        item2 = QTreeWidgetItem(item, ['Last released version'])
        self.itemDict[item2] = 'Last released version'        
        self.description[''] = "Description of "
        item3 = QTreeWidgetItem(item2, ['High resolution version'])
        self.itemDict[item3] = 'High resolution version'
        self.description['High resolution version'] = "Description of high resolution version"
        item3 = QTreeWidgetItem(item2, ['Low resolution version'])
        self.itemDict[item3] = 'Low resolution version'
        self.description['Low resolution version'] = "Description of low resolution version"
        item2 = QTreeWidgetItem(item, ['Stable version'])        
        self.itemDict[item2] = 'Stable version' 
        self.description[''] = "Description of "               
        item3 = QTreeWidgetItem(item2, ['High resolution version'])
        self.itemDict[item3] = 'High resolution version'
        self.description['High resolution version'] = "Description of high resolution version"
        item3 = QTreeWidgetItem(item2, ['Low resolution version'])
        self.itemDict[item3] = 'Low resolution version'
        self.description['Low resolution version'] = "Description of low resolution version"
        item = QTreeWidgetItem(parent, ['Old detector (256x304)'])
        self.itemDict[item] = 'Old detector (256x304)'        
        self.description[''] = "Description of "        
        item2 = QTreeWidgetItem(item, ['Last released version'])
        self.itemDict[item2] = 'Last released version'        
        self.description[''] = "Description of "
        item3 = QTreeWidgetItem(item2, ['High resolution version'])
        self.itemDict[item3] = 'High resolution version'
        self.description['High resolution version'] = "Description of high resolution version"
        item3 = QTreeWidgetItem(item2, ['Low resolution version'])
        self.itemDict[item3] = 'Low resolution version'
        self.description['Low resolution version'] = "Description of low resolution version"
        item2 = QTreeWidgetItem(item, ['Stable version'])        
        self.itemDict[item2] = 'Stable version' 
        self.description[''] = "Description of "               
        item3 = QTreeWidgetItem(item2, ['High resolution version'])
        self.itemDict[item3] = 'High resolution version'
        self.description['High resolution version'] = "Description of high resolution version"
        item3 = QTreeWidgetItem(item2, ['Low resolution version'])
        self.itemDict[item3] = 'Low resolution version'
        self.description['Low resolution version'] = "Description of low resolution version"
        ##REF_M
        parent = QTreeWidgetItem(ancestor, ['REF_M'])
        self.itemDict[parent] = 'REF_M'        
        self.description['REF_M'] = "Description of "        
        item = QTreeWidgetItem(parent, ['New detector (128x128)'])
        self.itemDict[item] = 'New detector (128x128)'        
        self.description['New detector (128x128)'] = "Description of "        
        item2 = QTreeWidgetItem(item, ['Last released version'])
        self.itemDict[item2] = 'Last released version'        
        self.description['Last released version'] = "Description of "
        item3 = QTreeWidgetItem(item2, ['High resolution version'])
        self.itemDict[item3] = 'High resolution version'
        self.description['High resolution version'] = "Description of high resolution version"
        item3 = QTreeWidgetItem(item2, ['Low resolution version'])
        self.itemDict[item3] = 'Low resolution version'
        self.description['Low resolution version'] = "Description of low resolution version"
        item2 = QTreeWidgetItem(item, ['Stable version'])        
        self.itemDict[item2] = 'Stable version' 
        self.description['Stable version'] = "Description of "               
        item3 = QTreeWidgetItem(item2, ['High resolution version'])
        self.itemDict[item3] = 'High resolution version'
        self.description['High resolution version'] = "Description of high resolution version"
        item3 = QTreeWidgetItem(item2, ['Low resolution version'])
        self.itemDict[item3] = 'Low resolution version'
        self.description['Low resolution version'] = "Description of low resolution version"
        item = QTreeWidgetItem(parent, ['Old detector (304x256)'])
        self.itemDict[item] = 'Old detector (304x256)'
        self.description['Old detector (304x256)'] = "Description of "                
        item2 = QTreeWidgetItem(item, ['Last released version'])
        self.itemDict[item2] = 'Last released version'
        self.description['Last released version'] = "Description of "                
        item3 = QTreeWidgetItem(item2, ['High resolution version'])
        self.itemDict[item3] = 'High resolution version'
        self.description['High resolution version'] = "Description of high resolution version"
        item3 = QTreeWidgetItem(item2, ['Low resolution version'])
        self.itemDict[item3] = 'Low resolution version'
        self.description['Low resolution version'] = "Description of low resolution version"
        item2 = QTreeWidgetItem(item, ['Stable version'])        
        self.itemDict[item2] = 'Stable version' 
        self.description['Stable version'] = "Description of "               
        item3 = QTreeWidgetItem(item2, ['High resolution version'])
        self.itemDict[item3] = 'High resolution version'
        self.description['High resolution version'] = "Description of high resolution version"
        item3 = QTreeWidgetItem(item2, ['Low resolution version'])
        self.itemDict[item3] = 'Low resolution version'
        self.description['Low resolution version'] = "Description of low resolution version"
        ##SEQUOIA
        parent = QTreeWidgetItem(ancestor, ['SEQUOIA'])
        self.itemDict[parent] = 'SEQUOIA'        
        self.description['SEQUOIA'] = "Description of DGSreduction"        
        
        ##Utilities        
        ancestor = QTreeWidgetItem(self.treeWidget, ['Utilities'])        
        self.itemDict[ancestor] = 'Utilities'        
        self.description[''] = "Description of "
        parent = QTreeWidgetItem(ancestor, ['CLoopES'])        
        self.itemDict[parent] = 'CLoopES'       
        self.description[''] = "Description of "         
        parent = QTreeWidgetItem(ancestor, ['DAD'])
        self.itemDict[parent] = 'DAD'        
        self.description[''] = "Description of "        
        parent = QTreeWidgetItem(ancestor, ['Geometry generator'])        
        self.itemDict[parent] = 'Geometry generator'
        self.description[''] = "Description of "                
        parent = QTreeWidgetItem(ancestor, ['MakeNeXus'])
        self.itemDict[parent] = 'MakeNeXus'     
        self.description[''] = "Description of "           

        ##Visualization
        ancestor = QTreeWidgetItem(self.treeWidget, ['Visualization'])
        self.itemDict[ancestor] = 'Visualization'        
        self.description[''] = "Description of "
        parent = QTreeWidgetItem(ancestor, ['ARCS'])
        self.itemDict[parent] = 'ARCS'        
        self.description[''] = "Description of "        
        parent = QTreeWidgetItem(ancestor, ['ASCII files'])        
        self.itemDict[parent] = 'ASCII files'   
        self.description[''] = "Description of "             
        parent = QTreeWidgetItem(ancestor, ['BSS'])
        self.itemDict[parent] = 'BSS'        
        self.description[''] = "Description of "        
        parent = QTreeWidgetItem(ancestor, ['CNCS'])                
        self.itemDict[parent] = 'CNCS'        
        self.description[''] = "Description of "        
        parent = QTreeWidgetItem(ancestor, ['All instruments'])
        self.itemDict[parent] = 'instruments'        
        self.description[''] = "Description of "
        parent = QTreeWidgetItem(ancestor, ['Region of interest (ROI)'])
        self.itemDict[parent] = 'Region of interest (ROI)'        
        self.description['Region of interest (ROI)'] = "Description of region of interest (ROI)"
            
#        ancestor = QTreeWidgetItem(self.treeWidget, ['Test'])

app = QApplication(sys.argv)
#    app.setApplicationName("Image Changer")
form = MainForm()
form.show()
app.exec_()


