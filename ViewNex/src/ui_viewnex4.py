# -*- coding: utf-8 -*-

# Form implementation generated from reading ui file './viewnex4.ui'
#
# Created: Wed Mar 23 13:53:44 2011
#      by: PyQt4 UI code generator 4.7.2
#
# WARNING! All changes made in this file will be lost!

from PyQt4 import QtCore, QtGui

class Ui_ViewNex(object):
    def setupUi(self, ViewNex):
        ViewNex.setObjectName("ViewNex")
        ViewNex.resize(550, 670)
        ViewNex.setMinimumSize(QtCore.QSize(550, 670))
        ViewNex.setMaximumSize(QtCore.QSize(550, 670))
        self.groupBox = QtGui.QGroupBox(ViewNex)
        self.groupBox.setGeometry(QtCore.QRect(10, 10, 532, 172))
        self.groupBox.setObjectName("groupBox")
        self.Browse_button = QtGui.QPushButton(self.groupBox)
        self.Browse_button.setGeometry(QtCore.QRect(11, 132, 511, 29))
        self.Browse_button.setObjectName("Browse_button")
        self.textLabel6_11 = QtGui.QLabel(self.groupBox)
        self.textLabel6_11.setGeometry(QtCore.QRect(11, 106, 88, 17))
        self.textLabel6_11.setText("")
        self.textLabel6_11.setWordWrap(False)
        self.textLabel6_11.setObjectName("textLabel6_11")
        self.Run_Number_label = QtGui.QLabel(self.groupBox)
        self.Run_Number_label.setGeometry(QtCore.QRect(20, 20, 82, 44))
        self.Run_Number_label.setWordWrap(False)
        self.Run_Number_label.setObjectName("Run_Number_label")
        self.Run_Number_Input = QtGui.QLineEdit(self.groupBox)
        self.Run_Number_Input.setGeometry(QtCore.QRect(100, 30, 164, 25))
        self.Run_Number_Input.setObjectName("Run_Number_Input")
        self.Instrument_label = QtGui.QLabel(self.groupBox)
        self.Instrument_label.setGeometry(QtCore.QRect(310, 20, 71, 44))
        self.Instrument_label.setWordWrap(False)
        self.Instrument_label.setObjectName("Instrument_label")
        self.OR = QtGui.QLabel(self.groupBox)
        self.OR.setGeometry(QtCore.QRect(260, 90, 71, 19))
        self.OR.setWordWrap(False)
        self.OR.setObjectName("OR")
        self.comboBox = QtGui.QComboBox(self.groupBox)
        self.comboBox.setGeometry(QtCore.QRect(390, 30, 111, 23))
        self.comboBox.setObjectName("comboBox")
        self.comboBox.addItem("")
        self.comboBox.addItem("")
        self.comboBox.addItem("")
        self.comboBox.addItem("")
        self.groupBox_2 = QtGui.QGroupBox(ViewNex)
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
        self.Time_start = QtGui.QLabel(self.groupBox_2)
        self.Time_start.setGeometry(QtCore.QRect(110, 30, 160, 21))
        self.Time_start.setText("")
        self.Time_start.setWordWrap(False)
        self.Time_start.setObjectName("Time_start")
        self.Proton_charge = QtGui.QLabel(self.groupBox_2)
        self.Proton_charge.setGeometry(QtCore.QRect(130, 70, 140, 21))
        self.Proton_charge.setText("")
        self.Proton_charge.setWordWrap(False)
        self.Proton_charge.setObjectName("Proton_charge")
        self.Duration_label = QtGui.QLabel(self.groupBox_2)
        self.Duration_label.setGeometry(QtCore.QRect(280, 30, 70, 21))
        self.Duration_label.setWordWrap(False)
        self.Duration_label.setObjectName("Duration_label")
        self.Total_counts_label = QtGui.QLabel(self.groupBox_2)
        self.Total_counts_label.setGeometry(QtCore.QRect(260, 70, 90, 21))
        self.Total_counts_label.setWordWrap(False)
        self.Total_counts_label.setObjectName("Total_counts_label")
        self.Duration = QtGui.QLabel(self.groupBox_2)
        self.Duration.setGeometry(QtCore.QRect(340, 20, 160, 21))
        self.Duration.setText("")
        self.Duration.setWordWrap(False)
        self.Duration.setObjectName("Duration")
        self.Total_counts = QtGui.QLabel(self.groupBox_2)
        self.Total_counts.setGeometry(QtCore.QRect(340, 70, 160, 21))
        self.Total_counts.setText("")
        self.Total_counts.setWordWrap(False)
        self.Total_counts.setObjectName("Total_counts")
        self.groupBox_3 = QtGui.QGroupBox(ViewNex)
        self.groupBox_3.setGeometry(QtCore.QRect(10, 330, 532, 112))
        self.groupBox_3.setObjectName("groupBox_3")
        self.Slit_1_label = QtGui.QLabel(self.groupBox_3)
        self.Slit_1_label.setGeometry(QtCore.QRect(70, 30, 40, 21))
        self.Slit_1_label.setWordWrap(False)
        self.Slit_1_label.setObjectName("Slit_1_label")
        self.Slit_1 = QtGui.QLabel(self.groupBox_3)
        self.Slit_1.setGeometry(QtCore.QRect(100, 30, 171, 21))
        self.Slit_1.setText("")
        self.Slit_1.setWordWrap(False)
        self.Slit_1.setObjectName("Slit_1")
        self.Slit_3_label = QtGui.QLabel(self.groupBox_3)
        self.Slit_3_label.setGeometry(QtCore.QRect(70, 70, 40, 21))
        self.Slit_3_label.setWordWrap(False)
        self.Slit_3_label.setObjectName("Slit_3_label")
        self.Slit_3 = QtGui.QLabel(self.groupBox_3)
        self.Slit_3.setGeometry(QtCore.QRect(100, 70, 171, 21))
        self.Slit_3.setText("")
        self.Slit_3.setWordWrap(False)
        self.Slit_3.setObjectName("Slit_3")
        self.Slit_2 = QtGui.QLabel(self.groupBox_3)
        self.Slit_2.setGeometry(QtCore.QRect(340, 30, 160, 21))
        self.Slit_2.setText("")
        self.Slit_2.setWordWrap(False)
        self.Slit_2.setObjectName("Slit_2")
        self.Slit_2_label = QtGui.QLabel(self.groupBox_3)
        self.Slit_2_label.setGeometry(QtCore.QRect(310, 30, 40, 21))
        self.Slit_2_label.setWordWrap(False)
        self.Slit_2_label.setObjectName("Slit_2_label")
        self.Slit_4_label = QtGui.QLabel(self.groupBox_3)
        self.Slit_4_label.setGeometry(QtCore.QRect(310, 70, 40, 21))
        self.Slit_4_label.setWordWrap(False)
        self.Slit_4_label.setObjectName("Slit_4_label")
        self.Slit_4 = QtGui.QLabel(self.groupBox_3)
        self.Slit_4.setGeometry(QtCore.QRect(350, 70, 160, 21))
        self.Slit_4.setText("")
        self.Slit_4.setWordWrap(False)
        self.Slit_4.setObjectName("Slit_4")
        self.groupBox_4 = QtGui.QGroupBox(ViewNex)
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
        self.ttheta = QtGui.QLabel(self.groupBox_4)
        self.ttheta.setGeometry(QtCore.QRect(110, 30, 171, 21))
        self.ttheta.setText("")
        self.ttheta.setWordWrap(False)
        self.ttheta.setObjectName("ttheta")
        self.ths = QtGui.QLabel(self.groupBox_4)
        self.ths.setGeometry(QtCore.QRect(110, 70, 171, 21))
        self.ths.setText("")
        self.ths.setWordWrap(False)
        self.ths.setObjectName("ths")
        self.tthd_label = QtGui.QLabel(self.groupBox_4)
        self.tthd_label.setGeometry(QtCore.QRect(310, 30, 37, 21))
        self.tthd_label.setWordWrap(False)
        self.tthd_label.setObjectName("tthd_label")
        self.thi_label = QtGui.QLabel(self.groupBox_4)
        self.thi_label.setGeometry(QtCore.QRect(320, 70, 28, 21))
        self.thi_label.setWordWrap(False)
        self.thi_label.setObjectName("thi_label")
        self.thi = QtGui.QLabel(self.groupBox_4)
        self.thi.setGeometry(QtCore.QRect(350, 70, 160, 21))
        self.thi.setText("")
        self.thi.setWordWrap(False)
        self.thi.setObjectName("thi")
        self.tthd = QtGui.QLabel(self.groupBox_4)
        self.tthd.setGeometry(QtCore.QRect(350, 30, 160, 21))
        self.tthd.setText("")
        self.tthd.setWordWrap(False)
        self.tthd.setObjectName("tthd")
        self.label = QtGui.QLabel(ViewNex)
        self.label.setGeometry(QtCore.QRect(10, 190, 91, 19))
        self.label.setObjectName("label")
        self.Full_file_name = QtGui.QLabel(ViewNex)
        self.Full_file_name.setGeometry(QtCore.QRect(100, 190, 441, 19))
        self.Full_file_name.setText("")
        self.Full_file_name.setObjectName("Full_file_name")

        self.retranslateUi(ViewNex)
        QtCore.QMetaObject.connectSlotsByName(ViewNex)

    def retranslateUi(self, ViewNex):
        ViewNex.setWindowTitle(QtGui.QApplication.translate("ViewNex", "Form1", None, QtGui.QApplication.UnicodeUTF8))
        self.groupBox.setTitle(QtGui.QApplication.translate("ViewNex", "Input", None, QtGui.QApplication.UnicodeUTF8))
        self.Browse_button.setText(QtGui.QApplication.translate("ViewNex", "Browse ...", None, QtGui.QApplication.UnicodeUTF8))
        self.Run_Number_label.setText(QtGui.QApplication.translate("ViewNex", "Run Number:", None, QtGui.QApplication.UnicodeUTF8))
        self.Instrument_label.setText(QtGui.QApplication.translate("ViewNex", "Instrument:", None, QtGui.QApplication.UnicodeUTF8))
        self.OR.setText(QtGui.QApplication.translate("ViewNex", "OR", None, QtGui.QApplication.UnicodeUTF8))
        self.comboBox.setItemText(0, QtGui.QApplication.translate("ViewNex", "EQSANS", None, QtGui.QApplication.UnicodeUTF8))
        self.comboBox.setItemText(1, QtGui.QApplication.translate("ViewNex", "REF_L", None, QtGui.QApplication.UnicodeUTF8))
        self.comboBox.setItemText(2, QtGui.QApplication.translate("ViewNex", "REF_M", None, QtGui.QApplication.UnicodeUTF8))
        self.comboBox.setItemText(3, QtGui.QApplication.translate("ViewNex", "Venus", None, QtGui.QApplication.UnicodeUTF8))
        self.groupBox_2.setTitle(QtGui.QApplication.translate("ViewNex", "Run infos", None, QtGui.QApplication.UnicodeUTF8))
        self.Time_start_label.setText(QtGui.QApplication.translate("ViewNex", "<p align=\"right\">Time start:</p>", None, QtGui.QApplication.UnicodeUTF8))
        self.Proton_charge_label.setText(QtGui.QApplication.translate("ViewNex", "<p align=\"right\">Proton charge:</p>", None, QtGui.QApplication.UnicodeUTF8))
        self.Duration_label.setText(QtGui.QApplication.translate("ViewNex", "<p align=\"right\">Duration:</p>", None, QtGui.QApplication.UnicodeUTF8))
        self.Total_counts_label.setText(QtGui.QApplication.translate("ViewNex", "<p align=\"right\">Total counts:</p>", None, QtGui.QApplication.UnicodeUTF8))
        self.groupBox_3.setTitle(QtGui.QApplication.translate("ViewNex", "Slits infos", None, QtGui.QApplication.UnicodeUTF8))
        self.Slit_1_label.setText(QtGui.QApplication.translate("ViewNex", "<p align=\"right\">Slit 1:</p>", None, QtGui.QApplication.UnicodeUTF8))
        self.Slit_3_label.setText(QtGui.QApplication.translate("ViewNex", "<p align=\"right\">Slit 3:</p>", None, QtGui.QApplication.UnicodeUTF8))
        self.Slit_2_label.setText(QtGui.QApplication.translate("ViewNex", "<p align=\"right\">Slit 2:</p>", None, QtGui.QApplication.UnicodeUTF8))
        self.Slit_4_label.setText(QtGui.QApplication.translate("ViewNex", "<p align=\"right\">Slit 4:</p>", None, QtGui.QApplication.UnicodeUTF8))
        self.groupBox_4.setTitle(QtGui.QApplication.translate("ViewNex", "Angles infos", None, QtGui.QApplication.UnicodeUTF8))
        self.ttheta_label.setText(QtGui.QApplication.translate("ViewNex", "<p align=\"right\">ttheta:</p>", None, QtGui.QApplication.UnicodeUTF8))
        self.ths_label.setText(QtGui.QApplication.translate("ViewNex", "<p align=\"right\">ths:</p>", None, QtGui.QApplication.UnicodeUTF8))
        self.tthd_label.setText(QtGui.QApplication.translate("ViewNex", "<p align=\"right\">tthd:</p>", None, QtGui.QApplication.UnicodeUTF8))
        self.thi_label.setText(QtGui.QApplication.translate("ViewNex", "<p align=\"right\">thi:</p>", None, QtGui.QApplication.UnicodeUTF8))
        self.label.setText(QtGui.QApplication.translate("ViewNex", "Full file name:", None, QtGui.QApplication.UnicodeUTF8))
