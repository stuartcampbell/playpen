from PyQt4.QtCore import *
from PyQt4.QtGui import *
from ui_viewnex import *
import sys
if __name__ == "__main__":
    app = QApplication(sys.argv)
    f = Ui_viewnex()
    f.show
    app.setMainWedget(f)
    app.exec_loop()
