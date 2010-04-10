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
    
    def __init__(self, parent=None):
        super(Sending, self).__init__(parent)
        
        self.browse = QPushButton("Browse Batch File(s)...")
#        self.label = QLabel("N/A")
        self.table = QTableView()

        self.send = QPushButton("SEND...")
        
        mainLayout = QVBoxLayout()
        mainLayout.addWidget(self.browse)
        mainLayout.addWidget(self.table)
        #mainLayout.addSpacing()
        
#        okButton = QPushButton("&OK")
        closeButton = QPushButton("CLOSE")
        sendingButton= QPushButton("SEND...")
        
        buttonLayout = QHBoxLayout()
        buttonLayout.addWidget(closeButton)   
        buttonLayout.addWidget(sendingButton)
        mainLayout.addLayout(buttonLayout)  
#        mainLayout.addWidget(closeButton)
        self.setLayout(mainLayout)
#        self.connect(okButton,SIGNAL("clicked()"), self, SLOT("accept()"))
        self.connect(closeButton, SIGNAL("clicked()"), self, SLOT("reject()"))
        self.setMinimumWidth(300)
        self.setWindowTitle("Sending Batch File")   

class Form(QDialog):

    def __init__(self, parent=None):
        super(Form, self).__init__(parent)

        self.sendButton = QPushButton("SEND")
        self.sendButton.setToolTip("Send 1 or more batch file(s) by email")
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
        
    def receive(self):
        print "Launch INSTALLING BATCH FILE form"
        self.receiveButton.setFocus()
    
app = QApplication(sys.argv)
form = Form()
form.show()
app.exec_()

