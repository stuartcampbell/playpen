import os
import sys
import time
from PyQt4 import QtCore
from PyQt4 import QtGui
import qrc_resources
from version import version as __version__
from logger import logger

class MainForm(QtGui.QDialog):

    def __init__(self, parent=None):
        super(MainForm, self).__init__(parent)
        
        self.itemDict = {}
        self.description = {}
        self.screen = {}
        self.exe = {}

        #left part of gui (widget tree)
        splitter = QtGui.QSplitter(QtCore.Qt.Horizontal)
        self.treeWidget = QtGui.QTreeWidget()
        splitter.addWidget(self.treeWidget)
        
        #right part (image, description and launch button)
        vLayout = QtGui.QVBoxLayout()
        self.image = QtGui.QLabel(self)
        self.descriptionWidget = QtGui.QLabel("")
        
        vLayout.addWidget(self.image)
        vLayout.addWidget(self.descriptionWidget)
        widget = QtGui.QWidget()
        widget.setLayout(vLayout)
        splitter.addWidget(widget)
        
        self.launch = QtGui.QPushButton("LAUNCH APPLICATION")
        self.launch.hide()
        self.help = QtGui.QPushButton("HELP APPLICATION")
        self.help.setMaximumWidth(200)
        h2Layout = QtGui.QHBoxLayout()
        h2Layout.addWidget(self.launch)        
        h2Layout.addWidget(self.help)
        
        #layout left and right part together
        gvLayout = QtGui.QVBoxLayout()
        gvLayout.addWidget(splitter)
        gvLayout.addLayout(h2Layout)

        self.setLayout(gvLayout)

        self.connect(self.treeWidget, QtCore.SIGNAL("itemClicked(QTreeWidgetItem*,int)"), self.tree_click_event)
        self.connect(self.treeWidget, QtCore.SIGNAL("itemSelectionChanged()"), self.tree_select_event)
        self.connect(self.treeWidget, QtCore.SIGNAL("itemDoubleClicked(QTreeWidgetItem*,int)"), self.launch_application)
        self.connect(self.launch, QtCore.SIGNAL("clicked()"), self.launch_application)
        self.connect(self.help, QtCore.SIGNAL("clicked()"), self.launch_help)

        self.setWindowTitle("SNS applications launcher (" + __version__ + ")")
        self.setMinimumWidth(1000)
        self.setMinimumHeight(500)
        
        QtCore.QTimer.singleShot(0, self.initialLoad)

    def launch_help(self):
        QtGui.QApplication.setOverrideCursor(QtCore.Qt.WaitCursor)
        os.system('/SNS/software/idltools/NeedHelp &')
        time.sleep(3)
        QtGui.QApplication.restoreOverrideCursor()

    def launch_application(self):
        QtGui.QApplication.setOverrideCursor(QtCore.Qt.WaitCursor)
        item = self.treeWidget.selectedItems()
        appli = self.itemDict[item[0]]
        os.system(self.exe[appli] + ' &')
        time.sleep(3)
        QtGui.QApplication.restoreOverrideCursor()

    def tree_select_event(self):
        item = self.treeWidget.selectedItems()
        appli = self.itemDict[item[0]]
        if self.description[appli] is not None:
            self.descriptionWidget.setText(self.description[appli])
        else:
            self.descriptionWidget.setText('')
        pixmap = QtGui.QPixmap(":/%s.gif" % self.screen[appli])
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
        pixmap = QtGui.QPixmap(":/%s.gif" % self.screen[appli])
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
        parent = QtGui.QTreeWidgetItem(ancestor, [local_label])
        self.itemDict[parent] = itemName
        if itemName not in self.description.keys():
            self.description[itemName] = description
            self.screen[itemName] = imageName
            self.exe[itemName] = exeName
        return parent
        
    def populateTree(self):
        #selected = None
        self.treeWidget.clear()
        self.treeWidget.setColumnCount(1)
        self.treeWidget.setHeaderLabel('List of applications sorted by:')
#        self.treeWidget.setHeaderHidden(True)
        self.treeWidget.setItemsExpandable(True)
        
        ##Application
        ancestor = self.createItem(self.treeWidget, 'Application',
                                   description='<html><b>List of all applications available.</b></html>')
        self.treeWidget.expandItem(ancestor)
        self.createItem(ancestor, "BatchTeleportation",
                        imageName='BatchTeleportation',
                        exeName='/SNS/software/idltools/pyqt/BatchTeleportation/BatchTeleportation',
                        description='This application can be used to send by email a Batch File from one user to another user.<br>' + 
                                 'The program takes care of attaching the right files with it and install them at the right place')
        self.createItem(ancestor, 'BSSreduction',
                        exeName='/SNS/software/idltools/BSSreduction',
                        imageName='BSSreduction',
                        description='Data Reduction for the <i>Backscattering</i> instrument <i>(BL2)</i>.')        
        self.createItem(ancestor, 'CLoopES',
                        exeName='/SNS/software/idltools/CLoopES',
                        imageName='CLoopES',
                        description='Command Line looper for elastic scans.')
        self.createItem(ancestor, 'DAD',
                        imageName='DAD',
                        exeName='/SNS/software/idltools/DAD',
                        description='Dave ascii division program')
        self.createItem(ancestor, 'DGSreduction',
                        imageName='DGSreduction',
                        exeName='/SNS/software/idltools/DGSreduction',
                        description='Data reduction for the direct geometry instruments ' + 
                        '<i>ARCS, CNCS, SEQUOIA and HYSPEC</i>.')
        self.createItem(ancestor, 'FITStools',
                        imageName='FITStools',
                        exeName='/SNS/software/idltools/FITStools',
                        description='FITS tools program used by <i>VENUS</i>.')
        self.createItem(ancestor, 'Geometry Generator',
                        imageName='GG',
                        exeName='/SNS/software/idltools/GG',
                        description='Program to generate temporary geometry files')
        self.createItem(ancestor, 'MakeNeXus',
                        imageName='MakeNeXus',
                        exeName='/SNS/software/bin/MakeNeXus.sh',
                        description='Program to produce new NeXus files with user defined histograming' + 
                        ' schema')
        self.createItem(ancestor, 'NeedHelp',
                        imageName='NeedHelp',
                        exeName='/SNS/software/idltools/NeedHelp',
                        description='Tool to quickly access some of our serviced and get help.')
        
        self.createItem(ancestor, 'plotARCS',
                        imageName='plotARCS',
                        exeName='/SNS/software/idltools/plotARCS',
                        description='Program to plot <i>ARCS</i> NeXus and event files')
        self.createItem(ancestor, 'plotASCII',
                        imageName='plotASCII',
                        exeName='/SNS/software/idltools/plotASCII',
                        description='Program to plot ASCII files produced by the various data reduction programs')
        self.createItem(ancestor, 'plotBSS',
                        imageName='plotBSS',
                        exeName='/SNS/software/idltools/plotBSS',
                        description='Program to plot NeXus and histogrammed files from <i>BSS</i>.')
        self.createItem(ancestor, 'plotCNCS',
                        imageName='plotCNCS',
                        exeName='/SNS/software/idltools/plotCNCS',
                        description='Program to plot NeXus and event file from the <i>CNCS</i> instrument.')
        self.createItem(ancestor, 'plotInstrument',
                        imageName='under_construction',
                        description='Program that will be able to plot any NeXus files from any instrument.')
        self.createItem(ancestor, 'plotROI',
                        imageName='plotROI',
                        exeName='/SNS/software/idltools/plotROI',
                        description='Program that plot any NeXus file and ROI files produced by the various ' + 
                        'data reduction programs.')
        self.createItem(ancestor, 'realignBSS',
                        exeName='/SNS/software/idltools/realignBSS',
                        imageName='realignBSS',
                        description='Program to visualize tubes/pixels/tof of the <i>BSS</i> instrument.')
        self.createItem(ancestor, 'REFoffSpec',
                        imageName='REFoffSpec',
                        exeName='/SNS/software/idltools/REFoffSpec',
                        description='Program to get specular and off-specular peaks from the <i>reflectometers</i>' + 
                        ' instruments.')
        parent = self.createItem(ancestor, 'REFreduction',
                                 description='Data reduction for the <i>reflectometers</i>.')
        item = self.createItem(parent, '1.3.x versions. Data reduction for REF_L (256x304) and REF_M (304x256).',
                               description='Data reduction for <i>REF_L</i> (256x304) and <i>REF_M</i> (304x256).')
        self.createItem(item, '1.3.x high resolution version',
                        exeName='/SNS/software/idltools/ref_reduction',
                        label="High resolution version",
                        imageName='REFreduction1_3',
                        description='High resolution version of the data reduction for <i>REF_L</i> (256x304) ' + 
                        'and <i>REF_M</i> (304x256).')
        self.createItem(item, '1.3.x low resolution version',
                        label='Low resolution version',
                        exeName='/SNS/software/idltools/mini_ref_reduction',
                        imageName='miniREFreduction1_3',
                        description='Low resolution version of the data reduction for <i>REF_L</i> (256x304) ' + 
                        'and <i>REF_M</i> (304x256).')
        item = self.createItem(parent, '1.5.x versions. Data reduction for the REF_L with rotated detector (304x256).',
                               description='Data reduction for the <i>REF_L</i> with ' + 
                               'rotated detector (304x256).')
        self.createItem(item, '1.5.x high resolution version',
                        label='High resolution version',
                        exeName='/SNS/software/idltools/REFreduction_v15',
                        imageName='REFreduction1_5',
                        description='High resolution version of the data reduction for the <i>REF_L</i> ' + 
                        'with rotated detector (304x256).')                                
        self.createItem(item, '1.5.x low resolution version',
                        label='Low resolution version',
                        exeName='/SNS/software/idltools/miniREFreduction_v15',
                        imageName='miniREFreduction1_5',
                        description='Low resolution version of the data reduction for the <i>REF_L</i> ' + 
                        'with rotated detector (304x256).')                                
        item = self.createItem(parent, '1.6.x versions. Data Reduction for the REF_M with new 128x128 detector.',
                               description='Data Reduction for the <i>REF_M</i> with ' + 
                               'new 128x128 detector.')
        self.createItem(item, '1.6.x high resolution version',
                        label='High resolution version',
                        exeName='/SNS/software/idltools/REFreduction_v16',
                        imageName='REFreduction1_6',
                        description='High resolution version of the data Reduction for the <i>REF_M</i>' + 
                        ' with new 128x128 detector.')                                
        self.createItem(item, '1.6.x low resolution version',
                        exeName='/SNS/software/idltools/miniREFreduction_v16',
                        label='Low resolution version',
                        imageName='miniREFreduction1_6',
                        description='Low resolution version of the data Reduction for the <i>REF_M</i>' + 
                        ' with new 128x128 detector.')                                
        parent = self.createItem(ancestor, 'REFscale',
                                 imageName='REFscale',
                                 description='Program to merge the specular reflectivity profiles <br>' +
                                 'from the different angles and produce the full reflectivity profile.')
        self.createItem(parent, 'REFscale_l',
                        imageName = 'REFscale',
                        exeName = '/SNS/software/idltools/REFscale_l',
                        description = 'Program to merge the specular reflectivity profiles of the <br>' +
                                'liquids reflectometer from the different angles and produce the full <br>' +
                                'reflectivity profile.')
        self.createItem(parent, 'REFscale_m',
                        imageName = 'REFscale',
                        exeName = '/SNS/software/idltools/REFscale_m',
                        description = 'Program to merge the specular reflectivity profiles of the <br>' +
                                'magnetism reflectometer from the different angles and produce the full <br>' +
                                'reflectivity profile.')
        
        self.createItem(ancestor, 'REFscaleOFF',
                        imageName='REFscaleOFF',
                        exeName='/SNS/software/idltools/REFscaleOFF',
                        description='Program to scale and stitch rtof files')

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
        self.createItem(parent, 'DGSreduction')        
        self.createItem(parent, 'Geometry Generator')
        self.createItem(parent, 'MakeNeXus')
        self.createItem(parent, 'plotARCS')
        self.createItem(parent, 'plotROI')
        #BSS
        parent = self.createItem(ancestor, 'BSS')        
        self.createItem(parent, 'BSSreduction')
        self.createItem(parent, 'CLoopES')
        self.createItem(parent, 'DAD')        
        self.createItem(parent, 'Geometry Generator')
        self.createItem(parent, 'MakeNeXus')
        self.createItem(parent, 'plotBSS')
        self.createItem(parent, 'plotROI')        
        self.createItem(parent, 'realignBSS')
        #EQSANS
        parent = self.createItem(ancestor, 'EQSANS')        
        self.createItem(parent, 'Geometry Generator')
        self.createItem(parent, 'MakeNeXus')
        self.createItem(parent, 'plotROI')
        item = self.createItem(parent, 'SANSreduction') 
        self.createItem(item, 'SANSreduction high resolution version',
                        label='High resolution version')
        self.createItem(item, 'SANSreduction low resolution version',
                        label='Low resolution version')                               
        #CNCS
        parent = self.createItem(ancestor, 'CNCS')        
        self.createItem(parent, 'DGSreduction')
        self.createItem(parent, 'Geometry Generator')
        self.createItem(parent, 'MakeNeXus')
        self.createItem(parent, 'plotCNCS')
        self.createItem(parent, 'plotROI')        
        #REF_L
        parent = self.createItem(ancestor, 'REF_L')
        self.createItem(parent, "BatchTeleportation")
        self.createItem(parent, 'Geometry Generator')
        self.createItem(parent, 'MakeNeXus')
        self.createItem(parent, 'plotASCII')
        self.createItem(parent, 'plotROI')
        self.createItem(parent, 'REFoffSpec')
        item = self.createItem(parent, 'REFreduction')
        item2 = self.createItem(item, '1.3.x versions')
        self.createItem(item2, '1.3.x high resolution version',
                        label='High resolution version')
        self.createItem(item2, '1.3.x low resolution version',
                        label='Low resolution version')
        item2 = self.createItem(item, '1.5.x versions')
        self.createItem(item2, '1.5.x high resolution version',
                        label='High resolution version')
        self.createItem(item2, '1.5.x low resolution version',
                        label='Low resolution version')
        item = self.createItem(parent, 'REFscale (beta and stable)')
        self.createItem(item, "REFscale_l")
        self.createItem(item, "REFscale (backup version)",
                        imageName='REFscale',
                        exeName='/SNS/software/idltools/REFscale_backup',
                        description='Backup version of program to merge the specular <br> ' + 
                        'reflectivity profiles from the different angles and produce <br> ' + 
                        'the full reflectivity profile.')
        self.createItem(parent, 'REFscaleOFF')
        
        #REF_M
        parent = self.createItem(ancestor, 'REF_M')
        self.createItem(parent, "BatchTeleportation")        
        self.createItem(parent, 'Geometry Generator')
        self.createItem(parent, 'MakeNeXus')
        self.createItem(parent, 'plotASCII')
        self.createItem(parent, 'plotROI')
        self.createItem(parent, 'REFoffSpec')
        item = self.createItem(parent, 'REFreduction')
        item2 = self.createItem(item, '1.3.x versions')
        self.createItem(item2, 'High resolution version',
                        label='1.3.x high resolution version')
        self.createItem(item2, 'Low resolution version',
                        label='1.3.x low resolution version')
        item2 = self.createItem(item, '1.6.x versions')
        self.createItem(item2, '1.6.x high resolution version',
                        label='High resolution version')
        self.createItem(item2, '1.6.x low resolution version',
                        label='Low resolution version')
        self.createItem(parent, 'REFscale_m')
        self.createItem(parent, 'REFscaleOFF')
            
        #SEQUOIA
        parent = self.createItem(ancestor, 'SEQUOIA')
        self.createItem(parent, 'DGSreduction')
        self.createItem(parent, 'Geometry Generator')
        self.createItem(parent, 'MakeNeXus')
        self.createItem(parent, 'plotASCII')
        self.createItem(parent, 'plotROI')
        
        #VENUS
        parent = self.createItem(ancestor, 'VENUS')        
        self.createItem(parent, 'FITStools')        
        
        ##Reduction
        ancestor = self.createItem(self.treeWidget, 'Reduction')
        self.createItem(ancestor, "reduction ARCS",
                        label='ARCS',
                        description='Data Reduction for <i>ARCS</i>',
                        imageName='DGSreduction')
        self.createItem(ancestor, "reduction BSS",
                        label='BSS',
                        description='Data Reduction for <i>BSS</i>',
                        imageName='BSSreduction')        
        self.createItem(ancestor, "reduction CNCS",
                        label='CNCS',
                        description='Data Reduction for <i>CNCS</i>',
                        imageName='DGSreduction')
        parent = self.createItem(ancestor, "reduction EQSANS",
                                 description='Data Reduction for <i>EQSANS</i>',
                                 label='EQSANS')
        self.createItem(parent, 'SANSreduction high resolution version',
                        label='High resolution version')
        self.createItem(parent, 'SANSreduction low resolution version',
                        label='Low resolution version')
        
        self.createItem(ancestor, "reduction HYSPEC",
                        description='Data Reduction for <i>HYSPEC</i>',
                        label='HYSPEC',
                        imageName='DGSreduction')
        
        parent = self.createItem(ancestor, "reduction REF_L",
                                 description='Data Reduction for <i>REF_L</i>',
                                 label='REF_L')
        self.createItem(parent, 'REFoffSpec')
        item = self.createItem(parent, 'REFreduction')
        item2 = self.createItem(item, '1.3.x versions (old detector 256x304)')
        self.createItem(item2, '1.3.x high resolution version',
                        label='High resolution version')
        self.createItem(item2, '1.3.x low resolution version',
                        label='Low resolution version')
        item2 = self.createItem(item, '1.5.x versions (new detector 304x256)')
        self.createItem(item2, '1.5.x high resolution version',
                        label='High resolution version')
        self.createItem(item2, '1.5.x low resolution version',
                        label='Low resolution version')

        parent = self.createItem(ancestor, "reduction REF_M",
                                 description='Data Reduction for <i>REF_M</i>',
                                 label='REF_M')
        self.createItem(parent, 'REFoffSpec')
        item = self.createItem(parent, 'REFreduction')
        item2 = self.createItem(item, '1.3.x versions (old detector 304x256)')
        self.createItem(item2, '1.3.x high resolution version',
                        label='High resolution version')
        self.createItem(item2, '1.3.x low resolution version',
                        label='Low resolution version',
                        imageName='miniREFreduction1_3')
        item2 = self.createItem(item, '1.6.x versions (new detector 304x256)')
        self.createItem(item2, '1.6.x high resolution version',
                        label='High resolution version')
        self.createItem(item2, '1.6.x low resolution version',
                        label='Low resolution version',
                        imageName='miniREFreduction1_3')

        parent = self.createItem(ancestor, "reduction SEQUOIA",
                                 description='Data Reduction for <i>SEQUOIA</i>',
                                 label='SEQUOIA',
                                 imageName='DGSreduction')
                
        ##Utilities        
        ancestor = self.createItem(self.treeWidget, 'Utilities')        
        parent = self.createItem(ancestor, "BatchTeleportation")
        parent = self.createItem(ancestor, 'CLoopES')
        parent = self.createItem(ancestor, 'DAD')
        parent = self.createItem(ancestor, 'FITStools')
        parent = self.createItem(ancestor, 'Geometry Generator')
        parent = self.createItem(ancestor, 'MakeNeXus')
        parent = self.createItem(ancestor, 'NeedHelp')

        ##Visualization
        ancestor = self.createItem(self.treeWidget, 'Visualization')
        parent = self.createItem(ancestor, "plotARCS")
        parent = self.createItem(ancestor, "plotASCII")                
        parent = self.createItem(ancestor, "plotBSS")
        parent = self.createItem(ancestor, "plotCNCS")
        parent = self.createItem(ancestor, "plotInstruments",
                                 imageName='under_construction')    
        parent = self.createItem(ancestor, "plotROI")

        ancestor = self.createItem(self.treeWidget, '')

        ##Beta version
        ancestor = self.createItem(self.treeWidget, 'Beta Version',
                                   description='Various Beta Version of some of our applications. <br>' + 
                                   'Chances that it will crash are pretty high so just be aware !')
                                   
        parent = self.createItem(ancestor, "CLoopES",
                                 exeName='/SNS/software/idltools/dev/CLoopES_beta',
                                 description='Beta version of CLoopES to test paralle processing of jobs')
        parent = self.createItem(ancestor, "DGSreduction beta version",
                                imageName='DGSreduction',
                                exeName='/SNS/software/idltools/DGSreduction-dev',
                                 description='Beta version of the data reduction for the direct geometry' + 
                                 ' instruments <i>ARCS, CNCS, <br>SEQUOIA and HYSPEC</i>.' + 
                                 '<br><font color=red>This is only for beta tester as it may crash')
        parent = self.createItem(ancestor, "REFoffSpec beta version",
                                 imageName='REFoffSpec',
                                 exeName='/SNS/software/idltools/beta/REFoffSpec_beta',
                                 description='Beta version of the REFoffSpec<br>' + 
                                 '<br><font color=red>This is only for beta tester as it may crash</font>' + 
                                 '<br><br>This beta version is currently for the REF_M team')       
        parent = self.createItem(ancestor, "REFreduction 1.5 beta version",
                                 imageName = 'REFreduction',
                                 description='Beta version of REFreduction<br>' + 
                                 '<br><font color=red>This is only for beta tester as it may crash</font>')
        self.createItem(parent, "High resolution version 1.5 beta",
                        imageName = 'REFreduction1_5',
                        exeName = '/SNS/software/idltools/beta/REFreduction_v15_beta',
                        description='Beta version of REFreduction<br>' + 
                                 '<br><font color=red>This is only for beta tester as it may crash</font>')
        self.createItem(parent, 'Low resolution version 1.5 beta',
                        imageName = 'miniREFreduction1_5',
                        exeName = '/SNS/software/idltools/beta/miniREFreduction_v15_beta',
                        description='Beta version of mini REFreduction<br>' + 
                                 '<br><font color=red>This is only for beta tester as it may crash</font>')

        ##developer
#        ancestor = self.createItem(self.treeWidget, 'Developer only !',
#                                       description = 'If you are not a developer, my advise would be to stay away from this branch  ;-) !')
#        parent = self.createItem(ancestor, "REFreduction 1.3.x debugging version",
#                                imageName='REFreduction1_3',
#                                exeName='/SNS/software/idltools/dev/ref_reduction_test',
#                                description='Debugging version of the data reduction for <i>REF_L</i> (256x304) ' + 
#                                'and <i>REF_M</i> (304x256).<br><font color=red>This is only for developers')


logger(application='SNStools') 
app = QtGui.QApplication(sys.argv)
form = MainForm()
form.show()
app.exec_()
