# Makefile for SNStools

# make : builds the executable via the idl virtual machine
# make install : deploys the program as main version

all:
	pyrcc4 -o qrc_resources.py resources.qrc

install:
	pyrcc4 -o qrc_resources.py resources.qrc
	cp qrc_resources.py /SNS/software/idltools/pyqt/SNStools/
	cp images/*.gif /SNS/software/idltools/pyqt/SNStools/images/
	cp *.pyw /SNS/software/idltools/pyqt/SNStools/
