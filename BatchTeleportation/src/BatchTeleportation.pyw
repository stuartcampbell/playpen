#!/usr/bin/env python
# Copyright (c) 2007-8 Qtrac Ltd. All rights reserved.
# This program or module is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as published
# by the Free Software Foundation, either version 2 of the License, or
# version 3 of the License, or (at your option) any later version. It is
# provided for educational purposes and is distributed in the hope that
# it will be useful, but WITHOUT ANY WARRANTY; without even the implied
# warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See
# the GNU General Public License for more details.

import sys
#import os
from PyQt4.QtCore import *
from PyQt4.QtGui import *

class Sending(QDialog):
    
    TABLE_Y_MAX = 20
    
    def __init__(self, parent=None):
        super(Sending, self).__init__(parent)
        
        self.browse = QPushButton("Browse Batch File(s)...")

        file_name = ["file1", "file2", "file3"]
        self.listWidget = QListWidget()
        self.listWidget.addItems(file_name)
        self.listWidget.setCurrentRow(0)
        
        self.send = QPushButton("SEND...")
        
        mainLayout = QVBoxLayout()
        mainLayout.addWidget(self.browse)
        mainLayout.addWidget(self.listWidget)
        #mainLayout.addSpacing()
        
#        okButton = QPushButton("&OK")
        closeButton = QPushButton("CLOSE")
        sendingButton = QPushButton("SEND...")
        
        buttonLayout = QHBoxLayout()
        buttonLayout.addWidget(closeButton)   
        buttonLayout.addWidget(sendingButton)
        mainLayout.addLayout(buttonLayout)  
#        mainLayout.addWidget(closeButton)
        self.setLayout(mainLayout)
#        self.connect(okButton,SIGNAL("clicked()"), self, SLOT("accept()"))
        self.connect(self.browse, SIGNAL("clicked()"), self.browsing)
        self.connect(closeButton, SIGNAL("clicked()"), self, SLOT("reject()"))
        self.setMinimumWidth(300)
        self.setWindowTitle("Sending Batch Files")
                
        #set focus on SEND button
        sendingButton.setFocus()   

    def browsing(self):
#        if not self.okToContinue():
#            return
        file = QFileDialog.getOpenFileName(self, "Select a batch file to send", 
                                                     "/Users/j35/results/", "Text Files (*.txt)")
        print file
#        if dialog.exec_():
#            print "ok"
#            self.addRecentFile(self.filename)
#            self.image = QImage()
#            for action, check in self.resetableActions:
#                action.setChecked(check)
#            self.image = dialog.image()
#            self.filename = None
#            self.dirty = True
#            self.showImage()
#            self.sizeLabel.setText("%d x %d" % (self.image.width(),
#                                                self.image.height()))
#            self.updateStatus("Created new image")


class Form(QDialog):

    def __init__(self, parent=None):
        super(Form, self).__init__(parent)

        self.dirty = False

        self.sendButton = QPushButton("&SEND")
        self.sendButton.setToolTip("Send 1 or more batch file(s) by email")
        self.sendButton.setStatusTip("Sending Batch Files")
                
        self.receiveButton = QPushButton("RECEIVE")
        self.receiveButton.setToolTip("Install 1 or more batch file(s)")
        layout = QHBoxLayout()
        layout.addWidget(self.sendButton)
        layout.addWidget(self.receiveButton)
        self.setLayout(layout)
        self.setWindowTitle("Batch Files Teleportation")
        self.setMinimumWidth(300)

        self.connect(self.sendButton, SIGNAL("clicked()"), self.send)
        self.connect(self.receiveButton, SIGNAL("clicked()"), self.receive)
        
    def send(self):
        print "Launch SENDING BATCH FILE form" #not modal window
        self.sendButton.setFocus()
        
        dialog = Sending(self)
        if dialog.exec_():
            print "ok"
        #os.system('python Sending.pyw &')
        
    #this event is triggered when the application is quit
#    def closeEvent(self, event):
#        if self.okToContinue():
#            print 'ok to close'
#        else:
#            event.ignore()
        
    def receive(self):
        print "Launch INSTALLING BATCH FILE form"
        self.receiveButton.setFocus()
    
# ask the user if he wants to save or not unsaved data (ok, no or cancel)
#    def okToContinue(self):
#        reply = QMessageBox.question(self,
#                                     "Batch Teleportation - Unsaved Changes",
#                                     "Save unsaved changes?",
#                                     QMessageBox.Yes | QMessageBox.No | 
#                                     QMessageBox.Cancel)
#        if reply == QMessageBox.Cancel:
#            return False
#        elif reply == QMessageBox.Yes:
#            self.fileSave()
#        return True
    
app = QApplication(sys.argv)
form = Form()
form.show()
app.exec_()

