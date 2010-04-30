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
import os
import re
from PyQt4.QtCore import *
from PyQt4.QtGui import *

class Sending(QDialog):
    
    TABLE_Y_MAX = 20
    
    def __init__(self, parent=None):
        super(Sending, self).__init__(parent)
        
        self.browse = QPushButton("Browse for batch file(s) to send...")

        self.toSendTree = QTreeWidget()
        self.toSendTree.setColumnCount(1)
        self.toSendTree.setHeaderHidden(True)

        #vertical base for SEND and SETTINGS button
        self.sendButton = QPushButton("SEND ->")
        self.settingsButton = QPushButton("Settings...")
        vLayout = QVBoxLayout()
        vLayout.addWidget(self.sendButton)
        vLayout.addWidget(self.settingsButton)
        
        self.sentTree = QTreeView()

        mainLayout = QVBoxLayout()
        row2Layout = QHBoxLayout()
        row2Layout.addWidget(self.toSendTree)
        row2Layout.addLayout(vLayout)
        row2Layout.addWidget(self.sentTree)
        mainLayout.addWidget(self.browse)
        mainLayout.addLayout(row2Layout)
        
        closeButton = QPushButton("CLOSE")
        closeButton.setFixedWidth(200)

        buttonLayout = QHBoxLayout()
        buttonLayout.addWidget(closeButton)   
        mainLayout.addLayout(buttonLayout)  
#        mainLayout.addWidget(closeButton)
        self.setLayout(mainLayout)
#        self.connect(okButton,SIGNAL("clicked()"), self, SLOT("accept()"))
        self.connect(self.browse, SIGNAL("clicked()"), self.browsing)
        self.connect(closeButton, SIGNAL("clicked()"), self, SLOT("reject()"))
        self.setMinimumWidth(300)
        self.setWindowTitle("Sending Batch Files")
                
        #set focus on SEND button
        self.sendButton.setFocus()   

    def browsing(self):
        oFiles = QFileDialog.getOpenFileNames(self, "Select 1 or more batch file(s) to send", 
                                            "/Users/j35/results/", "Text files (*.txt *.dat)")
        self.toSendTree.clear()
        for file in oFiles:
            parent = self.add_file_to_tree(file)
            txt_files = self.get_output_files(file)
#            print(txt_files)
            self.add_txt_file_to_parent(parent, txt_files)

    def get_output_files(self,file):
        #
        #open and read file to retrieve name of output files
        #
        f = open(file,"r")
        text = f.read() 
        lines = text.split('\n')
        output_files=[]
        for line in lines:
            m = re.findall('--output=([^ ]*)',line)
            if m is not None:
                for i in range(len(m)):
                    output_files.append(m[i])
        return output_files

    def add_file_to_tree(self, file):
        parent = QTreeWidgetItem(self.toSendTree, [file])
        return parent

    def add_txt_file_to_parent(self,parent,txt_files):
        for txt_file in txt_files:
            item = QTreeWidgetItem(parent,[txt_file])
