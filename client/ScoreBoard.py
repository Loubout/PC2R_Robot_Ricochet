from PyQt4 import QtGui, QtCore
import re

class ScoreBoard(QtGui.QTextEdit):

    def __init__(self, parent=None):
        super(ScoreBoard, self).__init__(parent)

        self.setStyleSheet("background-color : orange;")
        self.turn_number = 0
        self.scores = {}
        self.setReadOnly(True)
        # self.setCursor(QtGui.QCursor(QtCore.Qt.BlankCursor))

        self.show()

    def addPlayer(self, name, score=None):
        if score is None:
            self.scores[name] = 'spectator'
        else:
            self.scores[name] = score

    def update_scores(self, params):
        # 6(saucisse,3)(brouette,0)

        self.turn_number = params[0]
        find_scores = re.findall('\w+,\d+', params[1:])
        for s in find_scores:
            k_v = s.split(",")
            name = k_v[0]
            score = int(k_v[1])
            self.scores[name] = score # insert

        self.display()

    def display(self):
        self.clear()
        self.append("THIS IS TURN #" + str(self.turn_number)) #server does not send turn number yet
        for name, score in self.scores.items():
            self.append(name + " : " + str(score))



