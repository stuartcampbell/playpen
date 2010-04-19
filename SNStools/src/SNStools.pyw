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
        self.descriptionWidget = QLabel("here is the description")
        self.launch = QPushButton("LAUNCH APPLICATION")
#        self.launch.setFixedHeight(30)
        vLayout.addWidget(self.image)
        vLayout.addWidget(self.descriptionWidget)
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

        self.connect(self.treeWidget, SIGNAL("itemClicked(QTreeWidgetItem*,int)"), self.tree_event)
        self.connect(self.launch, SIGNAL("clicked()"), self.launch_application)

#        self.setCentralWidget(self.mainSplitter)
        self.setWindowTitle("SNS applications launcher")
        self.setMinimumWidth(500)
        self.setMinimumHeight(300)
        QTimer.singleShot(0, self.initialLoad)

    def launch_application(self):
        print 'launching current selected application'
#        item = self.treeWidget.item

    def tree_event(self,item):
        appli = self.itemDict[item]
        self.descriptionWidget.setText(self.description[appli])

    def initialLoad(self):
        self.populateTree()

    def populateTree(self):
        selected = None
        self.treeWidget.clear()
        self.treeWidget.setColumnCount(1)
#        self.treeWidget.setHeaderLabels(["Type/Application"])
        self.treeWidget.setHeaderHidden(True)
        self.treeWidget.setItemsExpandable(True)
        
        self.itemDict = {}
        self.description = {}
        
        ##Application
        ancestor = QTreeWidgetItem(self.treeWidget, ['Application'])
        self.itemDict[ancestor] = 'Application'
        self.description['Application'] = "Description of Application"        
        self.treeWidget.expandItem(ancestor)
        parent = QTreeWidgetItem(ancestor, ['CLoopES'])        
        self.itemDict[parent] = 'CLoopES'
        self.description['CLoopES'] = "Description of CLoopES"
        parent = QTreeWidgetItem(ancestor, ['DAD'])
        self.itemDict[parent] = 'DAD'
        self.description['DAD'] = "Description of DAD"        
        parent = QTreeWidgetItem(ancestor, ['DGSreduction'])
        self.itemDict[parent] = 'DGSreduction'
        self.description['DGSreduction'] = "Description of DGSreduction"
        parent = QTreeWidgetItem(ancestor, ['FITStools'])
        self.itemDict[parent] = 'FITStools'        
        self.description['FITStools'] = "Description of FITStools"
        parent = QTreeWidgetItem(ancestor, ['GG'])
        self.itemDict[parent] = 'GG'        
        self.description['GG'] = "Description of GG"
        parent = QTreeWidgetItem(ancestor, ['MakeNeXus'])
        self.itemDict[parent] = 'MakeNeXus'
        self.description['MakeNeXus'] = "Description of MakeNeXus"                
        parent = QTreeWidgetItem(ancestor, ['plotARCS'])
        self.itemDict[parent] = 'plotARCS'
        self.description['plotARCS'] = "Description of plotARCS"                
        parent = QTreeWidgetItem(ancestor, ['plotASCII'])
        self.itemDict[parent] = 'plotASCII'
        self.description['plotASCII'] = "Description of plotASCII"                
#        self.treeWidget.setCurrentItem(parent)
        parent = QTreeWidgetItem(ancestor, ['plotBSS'])
        self.itemDict[parent] = 'plotBSS'
        self.description['plotBSS'] = "Description of plotBSS"                
        parent = QTreeWidgetItem(ancestor, ['plotCNCS'])
        self.itemDict[parent] = 'plotCNCS'        
        self.description['plotCNCS'] = "Description of plotCNCS"
        parent = QTreeWidgetItem(ancestor, ['plotInstrument'])
        self.itemDict[parent] = 'plotInstrument'        
        self.description['plotInstrument'] = "Description of plotInstrument"
        parent = QTreeWidgetItem(ancestor, ['plotROI'])
        self.itemDict[parent] = 'plotROI'        
        self.description['plotROI'] = "Description of plotROI"
        parent = QTreeWidgetItem(ancestor, ['REFoffSpec'])
        self.itemDict[parent] = 'REFoffSpec'        
        self.description['REFoffSpec'] = "Description of REFoffSpec"
        parent = QTreeWidgetItem(ancestor, ['REFreduction'])
        self.itemDict[parent] = 'REFreduction'        
        self.description['REFreduction'] = "Description of REFreduction"
        item = QTreeWidgetItem(parent, ['1.3.x versions'])
        self.itemDict[item] = '1.3.x versions'        
        self.description['1.3.x versions'] = "Description of 1.3.x versions"
        item2 = QTreeWidgetItem(item, ['High resolution version'])
        self.itemDict[item2] = 'High resolution version'
        self.description['High resolution version'] = "Description of high resolution version"
        item2 = QTreeWidgetItem(item, ['Low resolution version'])
        self.itemDict[item2] = 'Low resolution version'
        self.description['Low resolution version'] = "Description of low resolution version"
        item = QTreeWidgetItem(parent, ['1.4.x versions'])
        self.itemDict[item] = '1.4.x versions'
        self.description['1.4.x versions'] = "Description of 1.4.x versions"                
        item2 = QTreeWidgetItem(item, ['High resolution version'])
        self.itemDict[item2] = 'High resolution version'
        self.description['High resolution version'] = "Description of high resolution version"
        item2 = QTreeWidgetItem(item, ['Low resolution version'])
        self.itemDict[item2] = 'Low resolution version'
        self.description['Low resolution version'] = "Description of low resolution version"
        item = QTreeWidgetItem(parent, ['1.5.x versions'])                
        self.itemDict[item] = '1.5.x versions'        
        self.description['1.5.x versions'] = "Description of 1.5.x versions"
        item2 = QTreeWidgetItem(item, ['High resolution version'])
        self.itemDict[item2] = 'High resolution version'
        self.description['High resolution version'] = "Description of high resolution version"
        item2 = QTreeWidgetItem(item, ['Low resolution version'])
        self.itemDict[item2] = 'Low resolution version'
        self.description['Low resolution version'] = "Description of low resolution version"
        parent = QTreeWidgetItem(ancestor, ['REFscale'])        
        self.itemDict[parent] = 'REFscale'
        self.description['REFscale'] = "Description of REFscale"                
        parent = QTreeWidgetItem(ancestor, ['SANSreduction'])
        self.itemDict[parent] = 'SANSreduction'
        self.description['SANSreduction'] = "Description of SANSreduction"
        item = QTreeWidgetItem(parent, ['High resolution version'])
        self.itemDict[item] = 'High resolution version'
        self.description['High resolution version'] = "Description of high resolution version"
        item = QTreeWidgetItem(parent, ['Low resolution version'])
        self.itemDict[item] = 'Low resolution version'
        self.description['Low resolution version'] = "Description of low resolution version"
        
        ##Instrument
        ancestor = QTreeWidgetItem(self.treeWidget, ['Instruments'])
        self.itemDict[ancestor] = 'Instruments'        
        self.description['Instruments'] = "Description of list of Instruments"
        #ARCS
        parent = QTreeWidgetItem(ancestor, ['ARCS'])
        self.itemDict[parent] = 'ARCS'        
        self.description['ARCS'] = "Description of ARCS"
        item = QTreeWidgetItem(parent, ['Geometry generator']) #GG
        self.itemDict[item] = 'Geometry generator'
        self.description['Geometry generator'] = "Description of geometry generator"        
        item = QTreeWidgetItem(parent, ['plotARCS']) #plotARCS
        self.itemDict[item] = 'plotARCS'
        self.description['plotARCS'] = "Description of plotARCS" 
        item = QTreeWidgetItem(parent, ['plotROI']) #plotROI
        self.itemDict[item] = 'plotROI'
        self.description['plotROI'] = "Description of plotROI"                       
        #BSS
        parent = QTreeWidgetItem(ancestor, ['BSS'])
        self.itemDict[parent] = 'BSS'        
        self.description['BSS'] = "Description of BSS"
        item = QTreeWidgetItem(parent, ['BSSreduction']) #BSSreduction
        self.itemDict[item] = 'BSSreduction'
        self.description['BSSreduction'] = "Description of BSSreduction"
        item = QTreeWidgetItem(parent, ['CLoopES']) #CLoopES
        self.itemDict[item] = 'CLoopES'
        self.description['CLoopES'] = "Description of CLoopES"
        item = QTreeWidgetItem(parent, ['DAD']) #DAD
        self.itemDict[item] = 'DAD'
        self.description['DAD'] = "Description of DAD"
        item = QTreeWidgetItem(parent, ['Geometry generator']) #GG
        self.itemDict[item] = 'Geometry generator'
        self.description['Geometry generator'] = "Description of geometry generator"
        item = QTreeWidgetItem(parent, ['MakeNeXus']) #MakeNeXus
        self.itemDict[item] = 'MakeNeXus'
        self.description['MakeNeXus'] = "Description of MakeNeXus"
        item = QTreeWidgetItem(parent, ['plotBSS']) #plotBSS
        self.itemDict[item] = 'plotBSS'
        self.description['plotBSS'] = "Description of plotBSS"
        item = QTreeWidgetItem(parent, ['plotROI']) #plotROI
        self.itemDict[item] = 'plotROI'
        self.description['plotROI'] = "Description of plotROI"        
        item = QTreeWidgetItem(parent, ['realignBSS']) #realignBSS
        self.itemDict[item] = 'realignBSS'
        self.description['realignBSS'] = "Description of realignBSS"
        #EQSANS
        parent = QTreeWidgetItem(ancestor, ['EQSANS'])
        self.itemDict[parent] = 'EQSANS'        
        self.description['EQSANS'] = "Description of EQSANS"
        item = QTreeWidgetItem(parent, ['Geometry generator']) #GG
        self.itemDict[item] = 'Geometry generator'
        self.description['Geometry generator'] = "Description of geometry generator"
        item = QTreeWidgetItem(parent, ['MakeNeXus']) #MakeNeXus
        self.itemDict[item] = 'MakeNeXus'
        self.description['MakeNeXus'] = "Description of MakeNeXus"   
        item = QTreeWidgetItem(parent, ['plotROI']) #plotROI
        self.itemDict[item] = 'plotROI'
        self.description['plotROI'] = "Description of plotROI"
        item = QTreeWidgetItem(parent, ['SANSreduction']) #SANSreduction
        self.itemDict[item] = 'SANSreduction'
        self.description['SANSreduction'] = "Description of SANSreduction"
        #CNCS
        parent = QTreeWidgetItem(ancestor, ['CNCS'])
        self.itemDict[parent] = 'CNCS'        
        self.description['CNCS'] = "Description of CNCS"
        item = QTreeWidgetItem(parent, ['Geometry generator']) #GG
        self.itemDict[item] = 'Geometry generator'
        self.description['Geometry generator'] = "Description of geometry generator"        
        item = QTreeWidgetItem(parent, ['MakeNeXus'])
        self.itemDict[item] = 'MakeNeXus'
        self.description['MakeNeXus'] = "Description of MakeNeXus"                
        item = QTreeWidgetItem(parent, ['plotCNCS']) #plotCNCS
        self.itemDict[item] = 'plotCNCS'
        self.description['plotCNCS'] = "Description of plotCNCS"        
        item = QTreeWidgetItem(parent, ['plotROI']) #plotROI
        self.itemDict[item] = 'plotROI'
        self.description['plotROI'] = "Description of plotROI"        
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


