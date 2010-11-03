from PyQt4.QtCore import *
from PyQt4.QtGui import *
from ui_viewnex import *
import sys
if __name__ == "__main__":
    app = QApplication(sys.argv)
    f = Viewnex()
    f.show
    app.setMainWedget(f)
    app.exec_loop()
