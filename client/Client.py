__author__ = 'lboutin'
from PyQt4 import QtGui, QtCore, QtNetwork
from Gameboard import *
from ScoreBoard import *
from BiddingBoard import *
import sys

HOST = "localhost"
PORT = 2016
ADDR = (HOST, PORT)

WINDOW_WIDTH = 600
WINDOW_HEIGHT = 600

GRID_MARGIN_H = 16
GRID_MARGIN_V = 46

RED = 0
BLUE = 1
YELLOW = 2
GREEN = 3

TOP = 0
RIGHT = 1
BOTTOM = 2
LEFT = 3

OBJECTIVE_SCORE = 10


class mainWindow(QtGui.QWidget):
    def __init__(self, parent=None):
        super(mainWindow, self).__init__()
        self.setObjectName("main")
        self.setStyleSheet("""#main {background-color: #D1E4EB}""")


        self.game_phase = ""
        self.player_name = ""

        self.grid = Gameboard(self)
        self.grid.setMinimumHeight(SIZE*GRID_GAP + 4)
        self.grid.setMinimumWidth(SIZE*GRID_GAP + 4)


        # CHAT PANEL STUFF
        self.chatPanel = QtGui.QTextEdit(self)

        self.chatPanel.setMinimumHeight(360)
        self.chatPanel.setMaximumHeight(600)
        self.chatPanel.setMinimumWidth(300)
        self.chatPanel.setMaximumWidth(360)
        self.chatPanel.setReadOnly(True)

        self.chatPanel.setStyleSheet("""color: darkblue;
                                        background-color : lightgray;
                                        border-radius: 3px;
                                        border-style : solid;
                                        border-width: 5px;
                                        border-color : darkred;
                                    """)

        self.chatInput = QtGui.QLineEdit(self)
        self.chatInput.setMinimumWidth(300)
        self.chatInput.setMaximumWidth(360)


        layout = QtGui.QHBoxLayout()

        # MIDDLE PANEL STUFF
        grid_layout = QtGui.QVBoxLayout()
        title = QtGui.QLabel("Robot Ricochet", self)
        title.setStyleSheet("""
                            background-color: lightsteelblue;
                            font-family: Tahoma;
                            font-size: 42px;
                            font-weight : bolder;
                            color : black;
                            """)
        title.setAlignment(QtCore.Qt.AlignCenter)
        title.setMinimumWidth(200)
        title.setMinimumHeight(50)
        grid_layout.addWidget(title)
        grid_layout.setAlignment(QtCore.Qt.AlignTop)
        grid_layout.setContentsMargins(GRID_MARGIN_H, 0, GRID_MARGIN_H, 0)
        grid_layout.addWidget(self.grid)

        self.timer_bar = QtGui.QProgressBar(self)
        self.timer_bar.setInvertedAppearance(True)
        self.timer_bar.setStyleSheet("""
                                    QProgressBar {
                                    text-align : center;
                                    color: black;
                                    font-size:20px;
                                    border: 3px solid darkgreen;
                                    border-radius: 7px;}
                                    """)

        grid_layout.addWidget(self.timer_bar)
        timer = QtCore.QTimer()
        timer.setInterval(1000)
        timer.timeout.connect(self.timer_tick)
        self.phase_timer = timer
        self.active_player_label = QtGui.QLabel("")


        chat_layout = QtGui.QVBoxLayout()
        chat_layout.setAlignment(QtCore.Qt.AlignLeft)
        chat_layout.setSpacing(10)
        chat_layout.addWidget(self.chatPanel)
        chat_layout.addWidget(self.chatInput)


        self.chatInput.returnPressed.connect(self.send_message_chat)


        # SCOREBOARD STUFF

        play_layout = QtGui.QVBoxLayout()


        self.scoreboard = ScoreBoard(self)
        self.scoreboard.setMinimumHeight(300)
        self.scoreboard.setMaximumHeight(300)
        self.scoreboard.setMinimumWidth(200)

        self.bids_board = BiddingBoard(self)
        self.bids_board.setMinimumHeight(300)
        self.bids_board.setMaximumHeight(300)
        self.bids_board.setMinimumWidth(200)

        play_layout.addWidget(self.scoreboard)
        play_layout.addWidget(self.bids_board)

        self.bidding_label = QtGui.QLabel("Enter your bid")
        self.bidding_input = QtGui.QLineEdit()
        self.bidding_input.setMaxLength(2)

        self.bidding_input.returnPressed.connect(self.send_bid)

        play_layout.addWidget(self.bidding_label)
        play_layout.addWidget(self.bidding_input)
        play_layout.setAlignment(QtCore.Qt.AlignTop)

        self.disable_bidding_ui()


        # SOLUTION INPUT STUFF
        self.pick_robot_buttons = QtGui.QButtonGroup(self)
        robot_button_layout = QtGui.QHBoxLayout()
        red_robot = QtGui.QPushButton()
        blue_robot = QtGui.QPushButton()
        yellow_robot = QtGui.QPushButton()
        green_robot = QtGui.QPushButton()
        red_robot.setStyleSheet("max-width: 30px;background-color:red;")
        blue_robot.setStyleSheet("max-width: 30px;background-color:blue;")
        yellow_robot.setStyleSheet("max-width: 30px;background-color:yellow;")
        green_robot.setStyleSheet("max-width: 30px;background-color:green;")
        red_robot.setCheckable(True)
        blue_robot.setCheckable(True)
        yellow_robot.setCheckable(True)
        green_robot.setCheckable(True)
        self.pick_robot_buttons.addButton(red_robot)
        self.pick_robot_buttons.setId(red_robot, RED)
        self.pick_robot_buttons.addButton(blue_robot)
        self.pick_robot_buttons.setId(blue_robot, BLUE)
        self.pick_robot_buttons.addButton(yellow_robot)
        self.pick_robot_buttons.setId(yellow_robot, YELLOW)
        self.pick_robot_buttons.addButton(green_robot)
        self.pick_robot_buttons.setId(green_robot, GREEN)
        self.pick_robot_buttons.setExclusive(True)



        self.pick_direction_buttons = QtGui.QButtonGroup(self)
        dir_button_layout = QtGui.QHBoxLayout()
        to_the_top = QtGui.QPushButton('\u2191')
        to_the_right = QtGui.QPushButton("\u2192")
        to_the_bottom = QtGui.QPushButton("\u2193")
        to_the_left = QtGui.QPushButton("\u2190")
        to_the_top.setStyleSheet("QPushButton{max-width: 30px;}QPushButton:checked{background-color:black}")
        to_the_right.setStyleSheet("QPushButton{max-width: 30px;}QPushButton:checked{background-color:black}")
        to_the_bottom.setStyleSheet("QPushButton{max-width: 30px;}QPushButton:checked{background-color:black}")
        to_the_left.setStyleSheet("QPushButton{max-width: 30px;}QPushButton:checked{background-color:black}")

        to_the_top.setCheckable(True)
        to_the_right.setCheckable(True)
        to_the_bottom.setCheckable(True)
        to_the_left.setCheckable(True)


        self.pick_direction_buttons.addButton(to_the_top)
        self.pick_direction_buttons.addButton(to_the_right)
        self.pick_direction_buttons.addButton(to_the_bottom)
        self.pick_direction_buttons.addButton(to_the_left)
        # SET ID
        self.pick_direction_buttons.setId(to_the_top, TOP)
        self.pick_direction_buttons.setId(to_the_right, RIGHT)
        self.pick_direction_buttons.setId(to_the_bottom, BOTTOM)
        self.pick_direction_buttons.setId(to_the_left, LEFT)
        self.pick_direction_buttons.setExclusive(True)


        robot_button_layout.addWidget(red_robot)
        robot_button_layout.addWidget(blue_robot)
        robot_button_layout.addWidget(yellow_robot)
        robot_button_layout.addWidget(green_robot)

        dir_button_layout.addWidget(to_the_top)
        dir_button_layout.addWidget(to_the_right)
        dir_button_layout.addWidget(to_the_bottom)
        dir_button_layout.addWidget(to_the_left)
        grid_layout.addLayout(robot_button_layout)
        grid_layout.addLayout(dir_button_layout)

        add_move_button = QtGui.QPushButton("NEXT MOVE")
        add_move_button.setStyleSheet("width:40px")
        add_move_button.clicked.connect(self.add_move)



        cancel_move_button = QtGui.QPushButton("X")
        cancel_move_button.setStyleSheet("max-width:20px")
        cancel_move_button.clicked.connect(self.cancel_move)


        submit_solution_button = QtGui.QPushButton("SUBMIT")
        submit_solution_button.setStyleSheet("width:40px;")
        submit_solution_button.clicked.connect(self.submit_solution)
        dir_button_layout.addWidget(add_move_button)
        dir_button_layout.addWidget(submit_solution_button)


        # TESTING to ease up ui toggling
        self.pick_direction_buttons.addButton(add_move_button)
        self.pick_direction_buttons.addButton(cancel_move_button)
        self.pick_direction_buttons.addButton(submit_solution_button)


        self.display_solution = QtGui.QLabel()
        self.display_solution.setStyleSheet("background-color:white;border: 1px solid pink;")
        self.player_solution = ""

        self.disable_resolution_ui()

        robot_button_layout.addWidget(self.display_solution)
        robot_button_layout.addWidget(cancel_move_button)

        layout.addLayout(play_layout)  # left column
        layout.addLayout(grid_layout)  # center
        layout.addLayout(chat_layout)  # right

        self.setLayout(layout)

        self.setup_socket()






    def submit_solution(self):
        self.send("SOLUTION/"+self.player_name+"/"+self.player_solution)

    def cancel_move(self):
        if len(self.player_solution) >= 2:
            self.player_solution = self.player_solution[:-2]
            self.display_solution.setText(self.player_solution)

    def add_move(self):
        id_dir = self.pick_direction_buttons.checkedId()
        id_color = self.pick_robot_buttons.checkedId()

        if id_dir == -1 or id_color == -1 :
            print("ERROR NEED 2 BUTTON PRESSED")
            return

        c = ''
        if id_color == RED:
            c = 'R'
        elif id_color == BLUE:
            c = 'B'
        elif id_color == YELLOW:
            c = 'J'
        elif id_color == GREEN:
            c = 'V'

        d = ''
        if id_dir == TOP:
            d = 'H'
        elif id_dir == RIGHT:
            d = 'D'
        elif id_dir == BOTTOM:
            d = 'B'
        elif id_dir == LEFT:
            d = 'G'

        self.player_solution += c+d
        self.display_solution.setText(self.player_solution)



    def timer_tick(self):
        val = self.timer_bar.value()
        self.timer_bar.setValue(val+1)

    def init_reflection_timer(self):
        self.timer_bar.setValue(0)
        self.timer_bar.setRange(0, 300)
        self.timer_bar.setFormat("REFLECTION PHASE : %vs/%ms")
        self.phase_timer.start()

    def init_bidding_timer(self):
        self.timer_bar.setValue(0)
        self.timer_bar.setRange(0, 30)
        self.timer_bar.setFormat("BIDDING PHASE : %vs/%ms")
        self.phase_timer.start()

    def init_resolution_timer(self):
        self.timer_bar.setValue(0)
        self.timer_bar.setRange(0, 60)
        self.timer_bar.setFormat("RESOLUTION PHASE : %vs/%ms")
        self.phase_timer.start()

    def setup_socket(self):
        self.blockSize = 0
        self.tcpSocket = QtNetwork.QTcpSocket(self)
        self.tcpSocket.readyRead.connect(self.readMessage)
        self.tcpSocket.connected.connect(self.on_connected)
        self.tcpSocket.error.connect(self.display_connect_error)
        self.tcpSocket.connectToHost(HOST, PORT)

    def display_connect_error(self):
        error = QtGui.QErrorMessage(self)
        error.showMessage("Error : could not connect to server " + HOST + ":" + str(PORT))
        error.exec_()  # blocks until user closes the modal
        self.close()  # once the user closes the error window shut down the app


    def player_authentication(self):
        self.get_player_name()
        self.send("CONNEXION/"+self.player_name)


    def enable_bidding_ui(self):
        self.bidding_input.show()
        self.bidding_label.show()

    def disable_bidding_ui(self):
        self.bidding_input.hide()
        self.bidding_label.hide()




    def disable_resolution_ui(self):
        for b in self.pick_robot_buttons.buttons():
            b.setDisabled(True)
        for b in self.pick_direction_buttons.buttons():
            b.setDisabled(True)

    def enable_resolution_ui(self):
        for b in self.pick_robot_buttons.buttons():
            b.setDisabled(False)
        for b in self.pick_direction_buttons.buttons():
            b.setDisabled(False)


    def send_bid(self):
        bid = self.bidding_input.text()
        if bid.isdigit():
            if self.game_phase == "Reflection":
                self.send("TROUVE/"+self.player_name+"/"+bid)
            elif self.game_phase == "Bidding":
                self.send("ENCHERE/"+self.player_name+"/"+bid)
            self.bidding_input.clear()
            self.player_bid = int(bid)

    def send_message_chat(self):
        msg = self.chatInput.text()
        self.send("DIREATOUS/"+msg)
        self.chatInput.clear()

    def on_connected(self):
        print('Now connected to server')
        self.player_authentication()

    def readMessage(self):
        while self.tcpSocket.bytesAvailable():
            msg = str(self.tcpSocket.readLine(), "utf-8")
            print('received from server : ' + msg)
            self.handle_server_message(msg)

    # UGLY AS FUCK PYTHON Y U NO SWITCH STATEMENT
    def handle_server_message(self, msg):
        split_msg = msg.split("/")
        instr = split_msg[0]
        params = split_msg[1:]
        if instr == "SESSION":
            walls = params[0]
            self.grid.set_walls(walls)
            self.grid.repaint()
            # self.scoreboard # update scorebard
        elif instr == "VAINQUEUR":
            bilan = params[0]
            self.scoreboard.update_scores(bilan)
            self.announce_winner(bilan)

        elif instr == "BIENVENUE":
            self.initUI()
            self.scoreboard.addPlayer(self.player_name)

        elif instr == "CONNECTE":
            name = params[0].rstrip()
            self.chatPanel.append(name + " has connected !\n")
            self.scoreboard.addPlayer(name)

        elif instr == "SORTI":
            name = params[0].rstrip()
            self.chatPanel.append(name + " has disconnected..\n")

        elif instr == "CHAT":
            name = params[0].rstrip()
            text = params[1].rstrip()
            if name == self.player_name:
                name_style = "style='color:orangered;font-weight:bold;text-decoration:underline'"
            else:
                name_style = "style='color:darkblue;font-weight:bold;'"
            self.chatPanel.insertHtml("<span " + name_style + ">" + name + \
                                      "<span style='color:black;font-weight:normal;text-decoration:none'> : " + text + "</span></span><br>")


        elif instr == "SERVER":
            text = params[0].rstrip()
            self.chatPanel.insertHtml("<span style='color:red;font-style:italic'> SERVER :: " + text + "</span><br>")

        elif instr == "TOUR":
            self.grid.set_problem(params[0]) # remove ( ) as always
            self.scoreboard.update_scores(params[1])
            names = re.findall('[a-zA-Z]+', params[1]) # if the client is in the scoreboard : he is a participant
            self.game_phase = "Reflection"
            self.disable_resolution_ui()
            if self.player_name in names:
                self.bids_board.empty()
                self.enable_bidding_ui()
                self.init_reflection_timer()

        elif instr == "TUASTROUVE":
            self.chatPanel.append("Proposal accepted.. switching to bidding phase\n")
            if self.game_phase == "Reflection":
                self.game_phase = "Bidding"
                self.init_bidding_timer()

        elif instr == "ILATROUVE":
            name = params[0].rstrip()
            bid = int(params[1].rstrip())
            self.bids_board.add_bid(name, bid)

            if self.game_phase == "Reflection":
                self.game_phase = "Bidding"
                self.init_bidding_timer()

        elif instr == "ENDREFLEXION":
            self.game_phase = "Bidding"
            self.init_bidding_timer()

        elif instr == "TUENCHERE":
            self.bids_board.add_bid("Your bid", int(self.player_bid))

        elif instr == "ECHECENCHERE":
            name = params[0].rstrip()
            if name == self.player_name:
                # self.chatPanel.append("Your bid was refused : incoherent with your previous bid\n")
                self.bidding_label.setText("Your bid was refused : incoherent with your previous bid")
            else:
                # self.chatPanel.append("Your bid was refused : incoherent with " + name + "'s bid\n")
                self.bidding_label.setText("Your bid was refused : incoherent with " + name + "'s bid")

        elif instr == "ILENCHERE":
            name = params[0].rstrip()
            nb_moves = int(params[1].rstrip())
            if name != self.player_name:
                self.bids_board.add_bid(name, nb_moves)

        elif instr == "FINENCHERE":
            name = params[0].rstrip()
            self.active_player = name
            # maybe update so active player is highlighted in the bidding board
            self.active_player_label.setText(name)
            # need a method to set active user or something
            self.game_phase = "Resolution" # need to switch the ui and enable it if active player
            self.disable_bidding_ui()
            self.chatPanel.insertHtml("<span 'style=color:darkmagenta'>" + self.active_player + " shall send his solution</span><br>")
            if self.player_name == self.active_player:
                self.enable_resolution_ui()
            else:
                self.disable_resolution_ui() # just in case

            self.init_resolution_timer()

        elif instr == "SASOLUTION":
            name = params[0].rstrip()
            moves = params[1].rstrip()
            self.chatPanel.insertHtml("<span style='color:darkmagenta'>" + self.active_player + "'s proposal : " + moves + "</span><br>")
            self.grid.animate_solution(moves) # we animates no matter if it's correct
        elif instr == "BONNE":
            self.chatPanel.insertHtml("<span 'style=color:darkmagenta'>" + self.active_player + " has succeeded </span><br>")
            # need to put an end to the turn
        elif instr == "MAUVAISE":
            name = params[0].rstrip()
            self.chatPanel.insertHtml("<span 'style=color:purple'>"+ self.active_player + " has failed.. NEXT</span><br>")
            self.active_player = name
            self.active_player_label.setText(name)
            self.chatPanel.insertHtml("<span 'style=color:purple'>"+ name + " shall send his solution</span><br>")
            self.init_resolution_timer()
            if self.player_name == self.active_player:
                self.enable_resolution_ui()
            else:
                self.disable_resolution_ui()

        elif instr == "TROPLONG":
            name = params[0].rstrip()
            self.active_player = name
            self.active_player_label.setText(name)
            self.chatPanel.insertHtml("<span 'style=color:purple'>"+ self.active_player + " shall send his solution</span><br>")
            self.init_resolution_timer()
            if self.player_name == self.active_player:
                self.enable_resolution_ui()
            else:
                self.disable_resolution_ui()
        elif instr == "FINRESO":
            self.chatPanel.insertHtml("<span style='color:purple'>No more players.. end of turn</span><br>")


    def announce_winner(self, report):
        objective = re.escape(str(OBJECTIVE_SCORE)) # we build our regexp with the global variable
        regex_winner = r"\w+," + objective
        winner = re.findall(regex_winner, report)[0].split(',')[0]
        self.chatPanel.insertHtml("<span style='color:purple'>"+ str(winner).upper() + " HAS WON BITCHES !</span><br>")


    def send(self, data):
        self.tcpSocket.write(QtCore.QByteArray((data+"\n").encode()))

    def initUI(self):
        self.resize(WINDOW_WIDTH, WINDOW_HEIGHT)
        self.setWindowTitle("Robot Ricochet")
        self.show()

    def get_player_name(self):
        input_name, ok = QtGui.QInputDialog.getText(self, 'Welcome', 'Pick your name:')
        while input_name == "":
            input_name, ok = QtGui.QInputDialog.getText(self, 'Welcome', 'Try again.. Pick your name:')
        self.player_name = input_name

    def closeEvent(self, event):
        print("CLOSING APP")
        if self.player_name != "":
            self.send("SORT/"+self.player_name)




def main():
    app = QtGui.QApplication(sys.argv)
    w = mainWindow()
    w.show()
    sys.exit(app.exec_())


if __name__ == '__main__':
    main()