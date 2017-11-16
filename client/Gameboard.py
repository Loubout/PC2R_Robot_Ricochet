__author__ = 'lboutin'

SIZE = 16

from PyQt4 import QtGui, QtCore
import re
import time

GRID_SIZE = 16
GRID_GAP = 32

WINDOW_WIDTH = 16*GRID_GAP + 10
WINDOW_HEIGHT = 16*GRID_GAP + 10



class Gameboard(QtGui.QWidget):

    def __init__(self, parent=None):
        super(Gameboard, self).__init__(parent)

        # self.setStyleSheet("background-color : white")
        self.walls = []
        self.matrix = [[int('0000', 2) for j in range(SIZE)] for y in range(SIZE)]
        self.red_robot = None
        self.target = None
        self.green_robot = None
        self.yellow_robot = None
        self.blue_robot = None

        self.init_position = {}


        self.red_image = QtGui.QImage("red_robot.png")
        self.blue_image = QtGui.QImage("blue_robot.png")
        self.yellow_image = QtGui.QImage("yellow_robot.png")
        self.green_image = QtGui.QImage("green_robot.png")


        self.phase = ""

        self.setStyleSheet("""
            border: 20px solid black;
            border-radius: 10px;
            background-color: rgb(0, 0, 0);
             """)


    # returns the new coordinates for a particular move
    def calculate_next_position(self, move):
        robots_pos = [self.red_robot, self.blue_robot, self.yellow_robot, self.green_robot]

        c, d = move  # move should be a couple or a 2 elements list consisting of color and direction

        if c == 'R':
            moving_robot = robots_pos.pop(0)
        elif c == 'B':
            moving_robot = robots_pos.pop(1)
        elif c == 'J':
            moving_robot = robots_pos.pop(2)
        elif c == 'V':
            moving_robot = robots_pos.pop(3)

        x, y = moving_robot
        if d == 'H':
            while x > 0 and (not self.matrix[x][y] & int('1000', 2)) and (not (x-1, y) in robots_pos):
                x -= 1
        elif d == 'D':
            while (y < SIZE-1) and (not self.matrix[x][y] & int('0100', 2)) and (not (x, y+1) in robots_pos):
                y += 1
        elif d == 'B':
            while (x < SIZE - 1) and (not self.matrix[x][y] & int('0010', 2)) and (not (x+1, y) in robots_pos):
                x += 1
        elif d == 'G':
            while (y > 0) and (not self.matrix[x][y] & int('0001', 2)) and (not (x, y-1) in robots_pos):
                y -= 1

        return x, y


    # def move_robot(self, c, delta_x, delta_y):
    #     if c == 'R':
    #         x, y = self.red_robot
    #         self.red_robot = (x + delta_x, y + delta_y)
    #     elif c == 'B':
    #         x, y = self.blue_robot
    #         self.blue_robot = (x + delta_x, y + delta_y)
    #     elif c == 'J':
    #         x, y = self.yellow_robot
    #         self.yellow_robot = (x + delta_x, y + delta_y)
    #     elif c == 'V':
    #         x, y = self.green_robot
    #         self.green_robot = (x + delta_x, y + delta_y)


    def animate_solution(self, sol_str):
        moves = [sol_str[i:i+2] for i in range(0, len(sol_str), 2)]
        for m in moves:
            x, y = self.calculate_next_position(m)
            c, d = m
            if c == 'R':
                self.red_robot = (x, y)
            elif c == 'B':
                self.blue_robot = (x, y)
            elif c == 'J':
                self.yellow_robot = (x, y)
            elif c == 'V':
                self.green_robot = (x, y)
            self.repaint()
            time.sleep(0.5)


    def build_matrix(self):
        for w in self.walls:
            x, y, z = w.split(",")
            x = int(x)
            y = int(y)
            if z == 'H':
                self.matrix[x][y] |= int('1000', 2)
                if x > 0:
                    self.matrix[x-1][y] |= int('0010', 2)
            elif z == 'D':
                self.matrix[x][y] |= int('0100', 2)
                if y < SIZE:
                    self.matrix[x][y+1] |= int('0001', 2)
            elif z == 'B':
                self.matrix[x][y] |= int('0010', 2)
                if x < SIZE:
                    self.matrix[x+1][y] |= int('1000', 2)
            elif z == 'G':
                self.matrix[x][y] |= int('0001', 2)
                if y > 0:
                    self.matrix[x][y-1] |= int('0100', 2)


    def set_walls(self, walls_str):
        self.walls = re.findall('[0-9]+,[0-9]+,.', walls_str)
        self.build_matrix()


    def paintEvent(self, e):
        qp = QtGui.QPainter()
        qp.begin(self)
        self.draw_grid(qp)
        self.draw_all_walls(qp, self.walls)
        self.draw_target(qp, self.target)
        self.draw_robot(qp, self.red_robot, 'R')
        self.draw_robot(qp, self.blue_robot, 'B')
        self.draw_robot(qp, self.yellow_robot, 'J')
        self.draw_robot(qp, self.green_robot, 'V')

        qp.end()

    def draw_grid(self, qp):
        pen = QtGui.QPen(QtCore.Qt.black, 1, QtCore.Qt.SolidLine, QtCore.Qt.RoundCap, QtCore.Qt.RoundJoin)
        qp.setPen(pen)
        qp.fillRect(0, 0, SIZE*GRID_GAP, SIZE*GRID_GAP, QtCore.Qt.white)
        for i in range(GRID_SIZE+1):
            qp.drawLine(i*GRID_GAP, 0, i*GRID_GAP, GRID_SIZE*GRID_GAP)
            qp.drawLine(0, i*GRID_GAP, GRID_SIZE*GRID_GAP, i*GRID_GAP)

    def draw_all_walls(self, qp, wall_list):
        # wall_list should be a list of string "x, y , side"
        for w in wall_list:
            args = w.split(',')
            self.draw_wall(qp, int(args[0]), int(args[1]), args[2])

    def set_problem(self, params):
        problem = params[1:-1].split(",")
        self.red_robot = (int(problem.pop(0)), int(problem.pop(0)))
        self.blue_robot = (int(problem.pop(0)), int(problem.pop(0)))
        self.yellow_robot = (int(problem.pop(0)), int(problem.pop(0)))
        self.green_robot = (int(problem.pop(0)), int(problem.pop(0)))

        # we need to retain initial position so we can put back the original problem after a players fails
        self.init_position['R'] = self.red_robot
        self.init_position['B'] = self.blue_robot
        self.init_position['Y'] = self.yellow_robot
        self.init_position['V'] = self.green_robot


        self.target = (int(problem.pop(0)), int(problem.pop(0)), problem.pop(0).replace("'", ""))
        self.repaint()

    def reset_problem(self):
        self.red_robot = self.init_position['R']
        self.blue_robot = self.init_position['B']
        self.yellow_robot = self.init_position['Y']
        self.green_robot= self.init_position['V']
        self.repaint()

    def draw_wall(self, qp, x, y, side):

        pen = QtGui.QPen(QtCore.Qt.darkBlue, 7, QtCore.Qt.SolidLine, QtCore.Qt.RoundCap)
        qp.setPen(pen)

        if side == "G":
            qp.drawLine(y*GRID_GAP, x*GRID_GAP, y*GRID_GAP, (x + 1)*GRID_GAP)
        elif side == "D":
            qp.drawLine((y+1) * GRID_GAP, x*GRID_GAP, (y + 1) * GRID_GAP, (x+1) * GRID_GAP)
        elif side == "H":
            qp.drawLine(y * GRID_GAP, x*GRID_GAP, (y + 1) * GRID_GAP, x * GRID_GAP)
        elif side == "B":
            qp.drawLine(y * GRID_GAP, (x + 1) * GRID_GAP, (y+1) * GRID_GAP, (x + 1) * GRID_GAP)
        else:
            print("ERROR side = " + side)


    def draw_robot(self, qp, pos, c):
        # color R | B | J | V
        if pos is None:
            return
        x, y = pos
        pen = QtGui.QPen(QtCore.Qt.black, 3, QtCore.Qt.SolidLine)
        qp.setPen(pen)
        if c == 'R':
            robot = self.red_image
        elif c == 'B':
            robot = self.blue_image
        elif c == 'J':
            # brush.setColor(QtCore.Qt.yellow)
            robot = self.yellow_image
        elif c == 'V':
            robot = self.green_image

        qp.drawImage(QtCore.QRect(y*GRID_GAP, x*GRID_GAP, GRID_GAP, GRID_GAP), robot)

    def draw_target(self, qp, t):
        if t is None:
            return
        x, y, c = t
        pen = QtGui.QPen(QtCore.Qt.black, 1, QtCore.Qt.SolidLine)
        qp.setPen(pen)

        if c == 'R':
            # brush.setColor(QtCore.Qt.red)
            qp.setBrush(QtCore.Qt.red)
        elif c == 'B':
            # brush.setColor(QtCore.Qt.blue)
            qp.setBrush(QtCore.Qt.blue)
        elif c == 'J':
            # brush.setColor(QtCore.Qt.yellow)
            qp.setBrush(QtCore.Qt.yellow)
        elif c == 'V':
            # brush.setColor(QtCore.Qt.green)
            qp.setBrush(QtCore.Qt.green)


        qp.drawEllipse(QtCore.QRect(y*GRID_GAP, x*GRID_GAP, GRID_GAP, GRID_GAP))



def main():
    pass

if __name__ == '__main__':
    main()