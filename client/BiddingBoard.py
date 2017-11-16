__author__ = 'lboutin'


from PyQt4 import QtGui, QtCore
import operator

class BiddingBoard(QtGui.QTextEdit):

    def __init__(self, parent=None):
        super(BiddingBoard, self).__init__(parent)

        self.setStyleSheet("background-color : palegreen;")

        self.bids = {}
        self.setReadOnly(True)
        # self.setCursor(QtGui.QCursor(QtCore.Qt.BlankCursor))

        self.show()

    def add_bid(self, name, bid):
        self.bids[name] = bid
        self.update_bids()

    def update_bids(self):
        self.clear()
        sorted_bids = sorted(self.bids.items(), key=operator.itemgetter(1))
        for b in sorted_bids:
            name, val = b
            self.append(name + " : " + str(val))

    def empty(self):
        self.bids = {}