# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file './ViewNexDlg.ui'
#
# Created: Wed Feb 23 10:25:24 2011
#      by: PyQt4 UI code generator 4.7.2
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

class Ui_ViewNexDlg(object):
    def setupUi(self, ViewNexDlg):
        ViewNexDlg.setObjectName("ViewNexDlg")
        ViewNexDlg.resize(550, 670)
        ViewNexDlg.setMinimumSize(QtCore.QSize(550, 670))
        ViewNexDlg.setMaximumSize(QtCore.QSize(550, 670))
        self.groupBox = QtGui.QGroupBox(ViewNexDlg)
        self.groupBox.setGeometry(QtCore.QRect(10, 10, 532, 172))
        self.groupBox.setObjectName("groupBox")
        self.browseButton = QtGui.QPushButton(self.groupBox)
        self.browseButton.setGeometry(QtCore.QRect(20, 120, 111, 29))
        self.browseButton.setObjectName("browseButton")
        self.textLabel6_11 = QtGui.QLabel(self.groupBox)
        self.textLabel6_11.setGeometry(QtCore.QRect(11, 106, 88, 17))
        self.textLabel6_11.setText("")
        self.textLabel6_11.setWordWrap(False)
        self.textLabel6_11.setObjectName("textLabel6_11")
        self.runnumberLabel = QtGui.QLabel(self.groupBox)
        self.runnumberLabel.setGeometry(QtCore.QRect(20, 20, 82, 44))
        self.runnumberLabel.setWordWrap(False)
        self.runnumberLabel.setObjectName("runnumberLabel")
        self.runInfo = QtGui.QLineEdit(self.groupBox)
        self.runInfo.setGeometry(QtCore.QRect(100, 30, 131, 25))
        self.runInfo.setObjectName("runInfo")
        self.instrumentLabel = QtGui.QLabel(self.groupBox)
        self.instrumentLabel.setGeometry(QtCore.QRect(240, 20, 71, 41))
        self.instrumentLabel.setWordWrap(False)
        self.instrumentLabel.setObjectName("instrumentLabel")
        self.orLabel = QtGui.QLabel(self.groupBox)
        self.orLabel.setGeometry(QtCore.QRect(260, 80, 71, 19))
        self.orLabel.setWordWrap(False)
        self.orLabel.setObjectName("orLabel")
        self.instrumentInfo = QtGui.QComboBox(self.groupBox)
        self.instrumentInfo.setGeometry(QtCore.QRect(310, 30, 111, 23))
        self.instrumentInfo.setObjectName("instrumentInfo")
        self.instrumentInfo.addItem("")
        self.instrumentInfo.setItemText(0, "")
        self.instrumentInfo.addItem("")
        self.instrumentInfo.addItem("")
        self.instrumentInfo.addItem("")
        self.instrumentInfo.addItem("")
        self.instrumentInfo.addItem("")
        self.instrumentInfo.addItem("")
        self.instrumentInfo.addItem("")
        self.instrumentInfo.addItem("")
        self.instrumentInfo.addItem("")
        self.instrumentInfo.addItem("")
        self.instrumentInfo.addItem("")
        self.browseInfo = QtGui.QLineEdit(self.groupBox)
        self.browseInfo.setGeometry(QtCore.QRect(140, 120, 371, 25))
        self.browseInfo.setObjectName("browseInfo")
        self.searchButton = QtGui.QPushButton(self.groupBox)
        self.searchButton.setGeometry(QtCore.QRect(430, 30, 80, 29))
        self.searchButton.setObjectName("searchButton")
        self.groupBox_2 = QtGui.QGroupBox(ViewNexDlg)
        self.groupBox_2.setGeometry(QtCore.QRect(10, 210, 532, 111))
        self.groupBox_2.setObjectName("groupBox_2")
        self.Time_start_label = QtGui.QLabel(self.groupBox_2)
        self.Time_start_label.setGeometry(QtCore.QRect(40, 30, 70, 21))
        self.Time_start_label.setWordWrap(False)
        self.Time_start_label.setObjectName("Time_start_label")
        self.Proton_charge_label = QtGui.QLabel(self.groupBox_2)
        self.Proton_charge_label.setGeometry(QtCore.QRect(10, 70, 100, 21))
        self.Proton_charge_label.setWordWrap(False)
        self.Proton_charge_label.setObjectName("Proton_charge_label")
        self.Duration_label = QtGui.QLabel(self.groupBox_2)
        self.Duration_label.setGeometry(QtCore.QRect(280, 30, 70, 21))
        self.Duration_label.setWordWrap(False)
        self.Duration_label.setObjectName("Duration_label")
        self.Total_counts_label = QtGui.QLabel(self.groupBox_2)
        self.Total_counts_label.setGeometry(QtCore.QRect(260, 70, 90, 21))
        self.Total_counts_label.setWordWrap(False)
        self.Total_counts_label.setObjectName("Total_counts_label")
        self.timestartInfo = QtGui.QLabel(self.groupBox_2)
        self.timestartInfo.setGeometry(QtCore.QRect(110, 30, 151, 25))
        self.timestartInfo.setObjectName("timestartInfo")
        self.protonchargeInfo = QtGui.QLineEdit(self.groupBox_2)
        self.protonchargeInfo.setGeometry(QtCore.QRect(110, 70, 151, 25))
        self.protonchargeInfo.setObjectName("protonchargeInfo")
        self.durationInfo = QtGui.QLineEdit(self.groupBox_2)
        self.durationInfo.setGeometry(QtCore.QRect(350, 30, 151, 25))
        self.durationInfo.setObjectName("durationInfo")
        self.totalcountsInfo = QtGui.QLineEdit(self.groupBox_2)
        self.totalcountsInfo.setGeometry(QtCore.QRect(350, 70, 151, 25))
        self.totalcountsInfo.setObjectName("totalcountsInfo")
        self.groupBox_3 = QtGui.QGroupBox(ViewNexDlg)
        self.groupBox_3.setGeometry(QtCore.QRect(10, 330, 532, 112))
        self.groupBox_3.setObjectName("groupBox_3")
        self.Slit_1_label = QtGui.QLabel(self.groupBox_3)
        self.Slit_1_label.setGeometry(QtCore.QRect(70, 30, 40, 21))
        self.Slit_1_label.setWordWrap(False)
        self.Slit_1_label.setObjectName("Slit_1_label")
        self.Slit_3_label = QtGui.QLabel(self.groupBox_3)
        self.Slit_3_label.setGeometry(QtCore.QRect(70, 70, 40, 21))
        self.Slit_3_label.setWordWrap(False)
        self.Slit_3_label.setObjectName("Slit_3_label")
        self.Slit_2_label = QtGui.QLabel(self.groupBox_3)
        self.Slit_2_label.setGeometry(QtCore.QRect(310, 30, 40, 21))
        self.Slit_2_label.setWordWrap(False)
        self.Slit_2_label.setObjectName("Slit_2_label")
        self.Slit_4_label = QtGui.QLabel(self.groupBox_3)
        self.Slit_4_label.setGeometry(QtCore.QRect(310, 70, 40, 21))
        self.Slit_4_label.setWordWrap(False)
        self.Slit_4_label.setObjectName("Slit_4_label")
        self.slit1Info = QtGui.QLineEdit(self.groupBox_3)
        self.slit1Info.setGeometry(QtCore.QRect(110, 30, 151, 25))
        self.slit1Info.setObjectName("slit1Info")
        self.slit3Info = QtGui.QLineEdit(self.groupBox_3)
        self.slit3Info.setGeometry(QtCore.QRect(110, 70, 151, 25))
        self.slit3Info.setObjectName("slit3Info")
        self.slit2Info = QtGui.QLineEdit(self.groupBox_3)
        self.slit2Info.setGeometry(QtCore.QRect(350, 30, 151, 25))
        self.slit2Info.setObjectName("slit2Info")
        self.slit4Info = QtGui.QLineEdit(self.groupBox_3)
        self.slit4Info.setGeometry(QtCore.QRect(350, 70, 151, 25))
        self.slit4Info.setObjectName("slit4Info")
        self.groupBox_4 = QtGui.QGroupBox(ViewNexDlg)
        self.groupBox_4.setGeometry(QtCore.QRect(10, 450, 532, 111))
        self.groupBox_4.setObjectName("groupBox_4")
        self.ttheta_label = QtGui.QLabel(self.groupBox_4)
        self.ttheta_label.setGeometry(QtCore.QRect(60, 30, 49, 21))
        self.ttheta_label.setWordWrap(False)
        self.ttheta_label.setObjectName("ttheta_label")
        self.ths_label = QtGui.QLabel(self.groupBox_4)
        self.ths_label.setGeometry(QtCore.QRect(80, 70, 32, 21))
        self.ths_label.setWordWrap(False)
        self.ths_label.setObjectName("ths_label")
        self.tthd_label = QtGui.QLabel(self.groupBox_4)
        self.tthd_label.setGeometry(QtCore.QRect(310, 30, 37, 21))
        self.tthd_label.setWordWrap(False)
        self.tthd_label.setObjectName("tthd_label")
        self.thi_label = QtGui.QLabel(self.groupBox_4)
        self.thi_label.setGeometry(QtCore.QRect(320, 70, 28, 21))
        self.thi_label.setWordWrap(False)
        self.thi_label.setObjectName("thi_label")
        self.tthetaInfo = QtGui.QLineEdit(self.groupBox_4)
        self.tthetaInfo.setGeometry(QtCore.QRect(110, 30, 151, 25))
        self.tthetaInfo.setObjectName("tthetaInfo")
        self.tthdInfo = QtGui.QLineEdit(self.groupBox_4)
        self.tthdInfo.setGeometry(QtCore.QRect(350, 30, 151, 25))
        self.tthdInfo.setObjectName("tthdInfo")
        self.thsInfo = QtGui.QLineEdit(self.groupBox_4)
        self.thsInfo.setGeometry(QtCore.QRect(110, 70, 151, 25))
        self.thsInfo.setObjectName("thsInfo")
        self.thiInfo = QtGui.QLineEdit(self.groupBox_4)
        self.thiInfo.setGeometry(QtCore.QRect(350, 70, 151, 25))
        self.thiInfo.setObjectName("thiInfo")
        self.fullfilenameLabel = QtGui.QLabel(ViewNexDlg)
        self.fullfilenameLabel.setGeometry(QtCore.QRect(10, 190, 91, 19))
        self.fullfilenameLabel.setObjectName("fullfilenameLabel")
        self.fullfilenameInfo = QtGui.QLabel(ViewNexDlg)
        self.fullfilenameInfo.setGeometry(QtCore.QRect(100, 190, 441, 33))
        self.fullfilenameInfo.setObjectName("fullfilenameInfo")

        self.retranslateUi(ViewNexDlg)
#        QtCore.QMetaObject.connectSlotsByName(ViewNexDlg)

        self.connect(self.searchButton, QtCore.SIGNAL("clicked()"), self.on_searchButton_clicked)
        self.connect(self.browseButton, QtCore.SIGNAL("clicked()"), self.on_browseButton_clicked)


    def retranslateUi(self, ViewNexDlg):
        ViewNexDlg.setWindowTitle(QtGui.QApplication.translate("ViewNexDlg", "Form1", None, QtGui.QApplication.UnicodeUTF8))
        self.groupBox.setTitle(QtGui.QApplication.translate("ViewNexDlg", "Input", None, QtGui.QApplication.UnicodeUTF8))
        self.browseButton.setText(QtGui.QApplication.translate("ViewNexDlg", "Browse ...", None, QtGui.QApplication.UnicodeUTF8))
        self.runnumberLabel.setText(QtGui.QApplication.translate("ViewNexDlg", "Run Number:", None, QtGui.QApplication.UnicodeUTF8))
        self.instrumentLabel.setText(QtGui.QApplication.translate("ViewNexDlg", "Instrument:", None, QtGui.QApplication.UnicodeUTF8))
        self.orLabel.setText(QtGui.QApplication.translate("ViewNexDlg", "OR", None, QtGui.QApplication.UnicodeUTF8))
        self.instrumentInfo.setItemText(1, QtGui.QApplication.translate("ViewNexDlg", "ARCS", None, QtGui.QApplication.UnicodeUTF8))
        self.instrumentInfo.setItemText(2, QtGui.QApplication.translate("ViewNexDlg", "BSS", None, QtGui.QApplication.UnicodeUTF8))
        self.instrumentInfo.setItemText(3, QtGui.QApplication.translate("ViewNexDlg", "CNCS", None, QtGui.QApplication.UnicodeUTF8))
        self.instrumentInfo.setItemText(4, QtGui.QApplication.translate("ViewNexDlg", "EQSANS", None, QtGui.QApplication.UnicodeUTF8))
        self.instrumentInfo.setItemText(5, QtGui.QApplication.translate("ViewNexDlg", "POWGEN", None, QtGui.QApplication.UnicodeUTF8))
        self.instrumentInfo.setItemText(6, QtGui.QApplication.translate("ViewNexDlg", "REF_L", None, QtGui.QApplication.UnicodeUTF8))
        self.instrumentInfo.setItemText(7, QtGui.QApplication.translate("ViewNexDlg", "REF_M", None, QtGui.QApplication.UnicodeUTF8))
        self.instrumentInfo.setItemText(8, QtGui.QApplication.translate("ViewNexDlg", "SNAP", None, QtGui.QApplication.UnicodeUTF8))
        self.instrumentInfo.setItemText(9, QtGui.QApplication.translate("ViewNexDlg", "SEQUOIA", None, QtGui.QApplication.UnicodeUTF8))
        self.instrumentInfo.setItemText(10, QtGui.QApplication.translate("ViewNexDlg", "TOPAZ", None, QtGui.QApplication.UnicodeUTF8))
        self.instrumentInfo.setItemText(11, QtGui.QApplication.translate("ViewNexDlg", "VULCAN", None, QtGui.QApplication.UnicodeUTF8))
        self.searchButton.setText(QtGui.QApplication.translate("ViewNexDlg", "Search", None, QtGui.QApplication.UnicodeUTF8))
        self.groupBox_2.setTitle(QtGui.QApplication.translate("ViewNexDlg", "Run infos", None, QtGui.QApplication.UnicodeUTF8))
        self.Time_start_label.setText(QtGui.QApplication.translate("ViewNexDlg", "<p align=\"right\">Time start:</p>", None, QtGui.QApplication.UnicodeUTF8))
        self.Proton_charge_label.setText(QtGui.QApplication.translate("ViewNexDlg", "<p align=\"right\">Proton charge:</p>", None, QtGui.QApplication.UnicodeUTF8))
        self.Duration_label.setText(QtGui.QApplication.translate("ViewNexDlg", "<p align=\"right\">Duration:</p>", None, QtGui.QApplication.UnicodeUTF8))
        self.Total_counts_label.setText(QtGui.QApplication.translate("ViewNexDlg", "<p align=\"right\">Total counts:</p>", None, QtGui.QApplication.UnicodeUTF8))
        self.groupBox_3.setTitle(QtGui.QApplication.translate("ViewNexDlg", "Slits infos", None, QtGui.QApplication.UnicodeUTF8))
        self.Slit_1_label.setText(QtGui.QApplication.translate("ViewNexDlg", "<p align=\"right\">Slit 1:</p>", None, QtGui.QApplication.UnicodeUTF8))
        self.Slit_3_label.setText(QtGui.QApplication.translate("ViewNexDlg", "<p align=\"right\">Slit 3:</p>", None, QtGui.QApplication.UnicodeUTF8))
        self.Slit_2_label.setText(QtGui.QApplication.translate("ViewNexDlg", "<p align=\"right\">Slit 2:</p>", None, QtGui.QApplication.UnicodeUTF8))
        self.Slit_4_label.setText(QtGui.QApplication.translate("ViewNexDlg", "<p align=\"right\">Slit 4:</p>", None, QtGui.QApplication.UnicodeUTF8))
        self.groupBox_4.setTitle(QtGui.QApplication.translate("ViewNexDlg", "Angles infos", None, QtGui.QApplication.UnicodeUTF8))
        self.ttheta_label.setText(QtGui.QApplication.translate("ViewNexDlg", "<p align=\"right\">ttheta:</p>", None, QtGui.QApplication.UnicodeUTF8))
        self.ths_label.setText(QtGui.QApplication.translate("ViewNexDlg", "<p align=\"right\">ths:</p>", None, QtGui.QApplication.UnicodeUTF8))
        self.tthd_label.setText(QtGui.QApplication.translate("ViewNexDlg", "<p align=\"right\">tthd:</p>", None, QtGui.QApplication.UnicodeUTF8))
        self.thi_label.setText(QtGui.QApplication.translate("ViewNexDlg", "<p align=\"right\">thi:</p>", None, QtGui.QApplication.UnicodeUTF8))
        self.fullfilenameLabel.setText(QtGui.QApplication.translate("ViewNexDlg", "Full file name:", None, QtGui.QApplication.UnicodeUTF8))

