import os
import sys
import time
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
        splitter = QSplitter(Qt.Horizontal)
        self.treeWidget = QTreeWidget()
        splitter.addWidget(self.treeWidget)
        
        #right part (image, description and launch button)
        vLayout = QVBoxLayout()
        self.image = QLabel(self)
        self.descriptionWidget = QLabel("")
        
        vLayout.addWidget(self.image)
        vLayout.addWidget(self.descriptionWidget)
        widget = QWidget()
        widget.setLayout(vLayout)
        splitter.addWidget(widget)
        
        self.launch = QPushButton("LAUNCH APPLICATION")
        self.launch.hide()
        self.help = QPushButton("HELP APPLICATION")
        self.help.setMaximumWidth(200)
        h2Layout = QHBoxLayout()
        h2Layout.addWidget(self.launch)        
        h2Layout.addWidget(self.help)
        
        #layout left and right part together
        gvLayout = QVBoxLayout()
        gvLayout.addWidget(splitter)
        gvLayout.addLayout(h2Layout)

        self.setLayout(gvLayout)

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
        QApplication.setOverrideCursor(Qt.WaitCursor)
        os.system('/SNS/software/idltools/NeedHelp &')
        time.sleep(3)
        QApplication.restoreOverrideCursor()

    def launch_application(self):
#        item = self.treeWidget.item
        QApplication.setOverrideCursor(Qt.WaitCursor)
        item = self.treeWidget.selectedItems()
        appli = self.itemDict[item[0]]
        os.system(self.exe[appli])
        time.sleep(3)
        QApplication.restoreOverrideCursor()

    def tree_select_event(self):
#        self.launch.setFocus()
        item = self.treeWidget.selectedItems()
        appli = self.itemDict[item[0]]
        if self.description[appli] is not None:
            self.descriptionWidget.setText(self.description[appli])
        else:
            self.descriptionWidget.setText('')
        pixmap = QPixmap(":/%s.gif" % self.screen[appli])
        self.image.setPixmap(pixmap)
        if self.exe[appli] is None:
            self.launch.hide()
        else:
            self.launch.show()

    def tree_click_event(self, item):
#        self.launch.setFocus()
        appli = self.itemDict[item]
        if self.description[appli] is not None:
            self.descriptionWidget.setText(self.description[appli])
        else:
            self.descriptionWidget.setText('')
        pixmap = QPixmap(":/%s.gif" % self.screen[appli])
        self.image.setPixmap(pixmap)
        if self.exe[appli] is None:
            self.launch.hide()
        else:
            self.launch.show()

    def initialLoad(self):
        self.populateTree()
        
    def createItem(self, ancestor, itemName, label=None, exeName=None, imageName=None, description=None):
        if label is None:
            local_label = itemName
        else:
            local_label = label
        parent = QTreeWidgetItem(ancestor, [local_label])
        self.itemDict[parent] = itemName
        if itemName not in self.description.keys():
            self.description[itemName] = description
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
        ancestor = self.createItem(self.treeWidget, 'Application',
                                   description='<html><b>List of all applications available.</b></html>')
        self.treeWidget.expandItem(ancestor)
        parent = self.createItem(ancestor, 'BSSreduction',
                                 exeName='/SNS/software/idltools/BSSreduction',
                                 imageName='BSSreduction',
                                 description='Data Reduction for the <i>Backscattering</i> instrument <i>(BL2)</i>.')        
        parent = self.createItem(ancestor, 'CLoopES',
                                 exeName='/SNS/software/idltools/CLoopES',
                                 imageName='CLoopES',
                                 description='Command Line looper for elastic scans.')
        parent = self.createItem(ancestor, 'DAD',
                                 imageName='DAD',
                                 exeName='/SNS/software/idltools/DAD',
                                 description='Dave ascii division program')
        parent = self.createItem(ancestor, 'DGSreduction',
                                 imageName='DGSreduction',
                                 exeName='/SNS/software/idltools/DGSreduction',
                                 description='Data reduction for the direct geometry instruments ' + 
                                 '<i>ARCS, CNCS, SEQUOIA and HYSPEC</i>.')
        parent = self.createItem(ancestor, 'FITStools',
                                 imageName='FITStools',
                                 exeName='/SNS/software/idltools/FITStools',
                                 description='FITS tools program used by <i>VENUS</i>.')
        parent = self.createItem(ancestor, 'Geometry Generator',
                                 imageName='GG',
                                 exeName='/SNS/software/idltools/GG',
                                 description='Program to generate temporary geometry files')
        parent = self.createItem(ancestor, 'MakeNeXus',
                                 imageName='MakeNeXus',
                                 exeName='/SNS/software/bin/MakeNeXus.sh',
                                 description='Program to produce new NeXus files with user defined histograming' + 
                                 ' schema')
        parent = self.createItem(ancestor, 'plotARCS',
                                 imageName='plotARCS',
                                 exeName='/SNS/software/idltools/plotARCS',
                                 description='Program to plot <i>ARCS</i> NeXus and event files')
        parent = self.createItem(ancestor, 'plotASCII',
                                 imageName='plotASCII',
                                 exeName='/SNS/software/idltools/plotASCII',
                                 description='Program to plot ASCII files produced by the various data reduction programs')
        parent = self.createItem(ancestor, 'plotBSS',
                                 imageName='plotBSS',
                                 exeName='/SNS/software/idltools/plotBSS',
                                 description='Program to plot NeXus and histogrammed files from <i>BSS</i>.')
        parent = self.createItem(ancestor, 'plotCNCS',
                                 imageName='plotCNCS',
                                 exeName='/SNS/software/idltools/plotCNCS',
                                 description='Program to plot NeXus and event file from the <i>CNCS</i> instrument.')
        parent = self.createItem(ancestor, 'plotInstrument',
                                 imageName='under_construction',
                                 description='Program that will be able to plot any NeXus files from any instrument.')
        parent = self.createItem(ancestor, 'plotROI',
                                 imageName='plotROI',
                                 exeName='/SNS/software/idltools/plotROI',
                                 description='Program that plot any NeXus file and ROI files produced by the various ' + 
                                 'data reduction programs.')
        parent = self.createItem(ancestor, 'realignBSS',
                                 exeName='/SNS/software/idltools/realignBSS',
                                 imageName='realignBSS',
                                 description='Program to visualize tubes/pixels/tof of the <i>BSS</i> instrument.')
        parent = self.createItem(ancestor, 'REFoffSpec',
                                 imageName='REFoffSpec',
                                 exeName='/SNS/software/idltools/REFoffSpec',
                                 description='Program to get specular and off-specular peaks from the <i>reflectometers</i>' + 
                                 ' instruments.')
        parent = self.createItem(ancestor, 'REFreduction',
                                 description='Data reduction for the <i>reflectometers</i>.')
        item = self.createItem(parent, '1.3.x versions',
                               description='Data reduction for <i>REF_L</i> (256x304) and <i>REF_M</i> (304x256).')
        item2 = self.createItem(item, '1.3.x high resolution version',
                                 exeName='/SNS/software/idltools/ref_reduction',
                                label="High resolution version",
                                imageName='REFreduction1_3',
                                description='High resolution version of the data reduction for <i>REF_L</i> (256x304) ' + 
                                'and <i>REF_M</i> (304x256).')
        item2 = self.createItem(item, '1.3.x low resolution version',
                                label='Low resolution version',
                                exeName='/SNS/software/idltools/mini_ref_reduction',
                                imageName='miniREFreduction1_3',
                                description='Low resolution version of the data reduction for <i>REF_L</i> (256x304) ' + 
                                'and <i>REF_M</i> (304x256).')
        item = self.createItem(parent, '1.5.x versions',
                               description='Data reduction for the <i>REF_L</i> with ' + 
                               'rotated detector (304x256).')
        item2 = self.createItem(item, '1.5.x high resolution version',
                                label='High resolution version',
                                exeName='/SNS/software/idltools/REFreduction_v15',
                                imageName='REFreduction1_5',
                                description='High resolution version of the data reduction for the <i>REF_L</i> ' + 
                                'with rotated detector (304x256).')                                
        item2 = self.createItem(item, '1.5.x low resolution version',
                                label='Low resolution version',
                                exeName='/SNS/software/idltools/miniREFreduction_v15',                                
                                imageName='miniREFreduction1_5',
                                description='Low resolution version of the data reduction for the <i>REF_L</i> ' + 
                                'with rotated detector (304x256).')                                
        item = self.createItem(parent, '1.6.x versions', 
                               description='Data Reduction for the <i>REF_M</i> with ' + 
                               'new 128x128 detector.')
        item2 = self.createItem(item, '1.6.x high resolution version',
                                label='High resolution version',
                                exeName='/SNS/software/idltools/REFreduction_v16',
                                imageName='REFreduction1_6',
                                description='High resolution version of the data Reduction for the <i>REF_M</i>' + 
                                ' with new 128x128 detector.')                                
        item2 = self.createItem(item, '1.6.x low resolution version',
                                exeName='/SNS/software/idltools/miniREFreduction_v16',
                                label='Low resolution version',
                                imageName='miniREFreduction1_6',
                                description='Low resolution version of the data Reduction for the <i>REF_M</i>' + 
                                ' with new 128x128 detector.')                                
        parent = self.createItem(ancestor, 'REFscale', 
                                 imageName='REFscale',
                                 exeName='/SNS/software/idltools/REFscale',
                                 description='Program to merge the specular reflectivity profiles from the ' + 
                                 'different angles and produce the full reflectivity profile.')
        parent = self.createItem(ancestor, 'SANSreduction', 
                                 description='Data reduction for <i>EQSANS</i>.')
        item = self.createItem(parent, 'SANSreduction high resolution version',
                               exeName='/SNS/software/idltools/SANSreduction',
                               label='High resolution version',
                               imageName='SANSreduction',
                               description='High resolution version of the ata reduction for <i>EQSANS</i>.')
        item = self.createItem(parent, 'SANSreduction low resolution version',
                               exeName='/SNS/software/idltools/miniSANSreduction',
                               label='Low resolution version',
                               imageName='miniSANSreduction',
                               description='Low resolution version of the ata reduction for <i>EQSANS</i>.')                               
        
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
                                label='ARCS',
                                description='Data Reduction for <i>ARCS</i>',
                                imageName='DGSreduction')
        parent = self.createItem(ancestor, "reduction BSS",
                                 label='BSS',
                                 description='Data Reduction for <i>BSS</i>',
                                 imageName='BSSreduction')        
        parent = self.createItem(ancestor, "reduction CNCS",
                                 label='CNCS',
                                 description='Data Reduction for <i>CNCS</i>',
                                 imageName='DGSreduction')
        parent = self.createItem(ancestor, "reduction EQSANS",
                                 description='Data Reduction for <i>EQSANS</i>',
                                 label='EQSANS')
        item = self.createItem(parent, 'SANSreduction high resolution version',
                               label='High resolution version')
        item = self.createItem(parent, 'SANSreduction low resolution version',
                               label='Low resolution version')
        
        parent = self.createItem(ancestor, "reduction HYSPEC",
                                 description='Data Reduction for <i>HYSPEC</i>',
                                 label='HYSPEC',
                                 imageName='DGSreduction')
        
        parent = self.createItem(ancestor, "reduction REF_L",
                                 description='Data Reduction for <i>REF_L</i>',
                                 label='REF_L')
        item = self.createItem(parent, 'REFoffSpec')
        item = self.createItem(parent, 'REFreduction')

        item2 = self.createItem(item, '1.3.x versions (old detector 256x304)')
        item3 = self.createItem(item2, '1.3.x high resolution version',
                                label='High resolution version')
        item4 = self.createItem(item3, '1.3.x low resolution version',
                                label='Low resolution version',
                                imageName='miniREFreduction1_3')
        item2 = self.createItem(item, '1.5.x versions (new detector 304x256)')
        item3 = self.createItem(item2, '1.5.x high resolution version',
                                label='High resolution version')
        item4 = self.createItem(item3, '1.5.x low resolution version',
                                label='Low resolution version',
                                imageName='miniREFreduction1_3')

        parent = self.createItem(ancestor, "reduction REF_M",
                                 description='Data Reduction for <i>REF_M</i>',
                                 label='REF_M')
        item = self.createItem(parent, 'REFoffSpec')
        item = self.createItem(parent, 'REFreduction')

        item2 = self.createItem(item, '1.3.x versions (old detector 304x256)')
        item3 = self.createItem(item2, '1.3.x high resolution version',
                                label='High resolution version')
        item4 = self.createItem(item3, '1.3.x low resolution version',
                                label='Low resolution version',
                                imageName='miniREFreduction1_3')
        item2 = self.createItem(item, '1.6.x versions (new detector 304x256)')
        item3 = self.createItem(item2, '1.6.x high resolution version',
                                label='High resolution version')
        item4 = self.createItem(item3, '1.6.x low resolution version',
                                label='Low resolution version',
                                imageName='miniREFreduction1_3')

        parent = self.createItem(ancestor, "reduction SEQUOIA",
                                 description='Data Reduction for <i>SEQUOIA</i>',
                                 label='SEQUOIA',
                                 imageName='DGSreduction')
                
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

        ancestor = self.createItem(self.treeWidget, '')

        ##Test
        ancestor = self.createItem(self.treeWidget, 'Developper')
        parent = self.createItem(ancestor, "REFreduction 1.3.x beta version",
                                imageName='REFreduction1_3',
                                exeName='/SNS/software/idltools/ref_reduction_backup',                                
                                description='Beta version of the data reduction for <i>REF_L</i> (256x304) ' + 
                                'and <i>REF_M</i> (304x256).<br><font color=red>This is only for beta tester as it may ' + 
                                'crash')
        parent = self.createItem(ancestor, "REFreduction 1.5.x beta version",
                                imageName='REFreduction1_5',
                                description='Beta version of the data reduction for the <i>REF_L</i> ' + 
                                'with rotated detector (304x256). <br><font color=red>This is only for beta tester as it may ' + 
                                'crash')                                
        parent = self.createItem(ancestor, "REFreduction 1.6.x beta version",
                                imageName='REFreduction1_6',
                                description='Beta version of the data Reduction for the <i>REF_M</i>' + 
                                ' with new 128x128 detector. <br><font color=red>This is only for beta tester as it may ' + 
                                'crash')                                                      
        parent = self.createItem(ancestor, "REFscale beta version",
                                imageName='REFscale',
                                 description='Beta version of the program that merges the specular reflectivity<br>' + 
                                 ' profiles from the different angles and produce the full reflectivity profile.' + 
                                 '<br><font color=red>This is only for beta tester as it may crash')                                                     
                
app = QApplication(sys.argv)
#    app.setApplicationName("Image Changer")
form = MainForm()
form.show()
app.exec_()


