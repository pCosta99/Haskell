import re
import itertools as it

########################### READER FUNCTIONS ##############################
def readBook(book):
    match = re.match(r"([a-z]+)([0-9]+)", book, re.I)
    return { "color": match.group(1), "number": match.group(2) }

def readMove(move):
    s = move.split("_")
    return { "from" : s[0], "to" : s[1] }

########################### HELPER FUNCTIONS ##############################

# Some extra functions that will be useful when developing the main functions
def cloneTower(tower):
    new_tower = []
    for i in range(len(tower)):
        new_tower.append(tower[i].copy())
    return new_tower

def fromStringMatrix(towers):
    return list(map(lambda col : list(map(readBook, col)), towers))

def validColumn(col):
    descending = all((int(earlier["number"]) == (int(later["number"]) + 1)) for earlier, later in zip(col, col[1:]))
    sameColor = len(col) == 0 or set(list(map(lambda b : b["color"], col)))
    return sameColor and descending

# checks if a column is fully connected (only one color and ordered)
# def fullyConnected(col)

# checks if two books match
def match(b1,b2):
    return b1["color"] == b2["color"] and (int(b1["number"]) + 1 == int(b2["number"]))

def longestValidSubSeq(column):
    new_col = column.copy()
    i = 0
    for i in reversed(range(0,len(new_col))):
        if not match(new_col[i], new_col[i-1]):
            break
    ret = new_col[i:len(new_col)].copy()
    return ret

# Checks if a move is valid
def isValid(fr, to):
    if(len(fr) == 0): return False
    elif(len(to) == 0): return True
    bottom = fr[0]
    top = to[len(to)-1]
    sameColor = bottom["color"] == top["color"]
    numberSafe = int(bottom["number"]) < int(top["number"])
    return (sameColor and numberSafe)

# Cleans up the valid moves, removing the ones that make no sense and giving priority to some specific others
# Dumb moves are:
# D1 - Moving a fully connected column into an open column
# D2 - ?
# Not smart (will lose priority) moves are:
# NS1 - Doing a movement that doesn't connect anything
def dumbMove(m, tower):
    return (validColumn(tower[m[2]]) and len(tower[m[3]]) == 0) or (validColumn(tower[m[3]]) and len(tower[m[2]]) == 0) # D1

def removeDumb(moves, tower):
    new_moves = moves.copy()
    # Take dumb moves out
    for m in moves:
        if(dumbMove(m,tower)): new_moves.remove(m)

    # Sort according to value
    new_moves.sort(reverse=True, key=moveValue)
    return new_moves

def moveValue(move):
    if(len(move[1]) == 0): return 2 # empty destiny pile
    elif(int(move[0][0]["number"]) + 1 == int(move[1][-1]["number"])): return 100 # Connecting has max value
    elif(move[0][0]["color"] == move[1][-1]["color"]): return 1 # connecting only in color
    else: return 0

####################### HISTORY RELATED FUNCTIONS #########################

def printHistory(history):
    print(len(history))
    for h in history:
        print((h[2],h[3]))

####################### TOWER RELATED FUNCTIONS ###########################
# checks if two towers are the same
def compareTower(t1,t2):
    if(len(t1) != len(t2)): return False
    cols = []
    for i in range(len(t1)):
        if(t1[i] != t2[i]): return False
    return True

# Checks if a tower is solved
def isSolved(tower):
    return all(list(map(validColumn, tower)))

# Returns the valid moves we can do for a certain tower setup
def allValidMoves(tower):
    n = range(len(tower))
    possibilities = list(it.combinations(n, 2)) + list(it.combinations(reversed(n), 2))
    # printTower(tower)
    moving = list(map(longestValidSubSeq, tower)) # stores the subsequence that is going to move for each column
    seq_poss = list(map(lambda xy : (moving[xy[0]],moving[xy[1]],xy[0],xy[1]), possibilities)) # combine moving with possibilities
    validMoves = list(filter(lambda xy : isValid(xy[0],xy[1]), seq_poss))
    return validMoves

def applyMove(tower, move):
    new_tower = cloneTower(tower)
    fr = move[2]
    to = move[3]
    new_tower[fr] = list(it.islice(new_tower[fr], len(new_tower[fr]) - len(move[0])))
    new_tower[to] = new_tower[to] + move[0]
    return new_tower

def printTower(tower):
    for col in tower:
        print(col)

####################### GRAPH RELATED FUNCTIONS ###########################

def createGraph(trs):
    solutions = []
    mem = []
    threshold = 1
    movementThreshold = 130
    createGraphRec(trs, [], [], solutions, threshold, movementThreshold)
    printHistory(solutions[0][0])
    return solutions[0][0]
    # solFstAcc = []
    # for l in solFst:
        # if (l not in solFstAcc):
            # solFstAcc.append(l)

    # print(len(solFstAcc))
    # print(len(solFst))

def progress(trs, moves):
    nt = trs.copy()
    for m in moves:
        print("MOVE:", m)
        input()
        # printTower(nt)
        nt = applyMove(nt, m)
    print(isSolved(nt))

def createGraphRec(trs, mem, history, solutions, threshold, movementThreshold):
    # breakpoint()
    moves = allValidMoves(trs)
    for move in removeDumb(moves, trs):
        branch_history = history.copy()
        new_tower = applyMove(trs, move)
        # breakpoint()
        branch_history.append(move)
        if(isSolved(new_tower)):
            # breakpoint()
            print("Apanhei uma!", len(history))
            if(len(history) < movementThreshold): solutions.append((branch_history,len(history)))
            break
        elif(not any(compareTower(new_tower, n) for n in mem)):
            mem.append(new_tower)
            if(len(solutions) < threshold): createGraphRec(new_tower,mem,branch_history,solutions, threshold, movementThreshold)
    # print("Branch finished.")

###########################################################################

# A possible setup of towers
string_trs0 = [["B6","B5","B4","B3","B2","B1","R0","B0"] # blue
               ,["R6","R5","R4","R3","R2","R1","R0","B2","B1"] # red
               ,["G6","G5","G4","G3","G2","G1","G0"] # green
               ,["P6","P5","P4","P3","P2","P1","P0"] # purple
              ]

string_trs1 = [["O5","O4","B3","B2","LB1","P2"],
               ["R2","R1","R0","P6","B4"],
               ["LB4","LB3","R3","B5","P1"],
               ["O6","B0","R6","R5","R4"],
               ["G6","G5","G4","LB2","P4"],
               ["O1","O0","B1","LB0","P5"],
               ["LB6","LB5","O3","O2","P3"],
               ["G3","G2","G1","G0","B6","P0"]
        ]

string_trs5 = [["LB2","B1","G5","R1","P1","O6"]
              ,["P6","O4","R3","O5","R5","G3"]
              ,["LB1","R0","R2","B5","O1"]
              ,["P5","LB3","O2","P4","B2"]
              ,["LB6","B6","G1","B4","R6"]
              ,["O0","LB0","G2","G6","G4"]
              ,["LB4","P3","O3","B3","G0"]
              ,["R4","P0","LB5","P2","B0"]]

string_trs6 = [["G5","B4","P2","B6","B2","P1"]
              ,["R1","O4","R2","P0","B3","G4"]
              ,["LB1","B1","P3","LB0","P5"]
              ,["R0","R6","P4","LB3","LB5"]
              ,["O2","LB6","R3","G1","G2"]
              ,["O5","O1","LB4","R5","G6"]
              ,["O6","B5","O0","G3","G0"]
              ,["P6","LB2","B0","R4","O3"]]

string_trs7 = [["O6","P2","B3","P3","G0","P0"]
              ,["R1","O0","LB4","G5","G6","G4"]
              ,["LB5","P5","R5","P1","B0"]
              ,["G1","P4","O1","B6","G2"]
              ,["LB1","G3","LB3","B1","R4"]
              ,["LB6","O4","LB2","O2","B2"]
              ,["R6","O5","LB0","O3","P6"]
              ,["R3","R0","B4","R2","B5"]]

ultra_bt = [["LB0","O4","P4","O5","LB1","B1"]
           ,["R4","R2","P3","G6","LG6","LG2"]
           ,["P2","G3","B4","P0","LB2","B6"]
           ,["B2","LG1","G5","O6","LB6","P1"]
           ,["LB5","P6","B0","R6","G1"]
           ,["LG4","R3","G4","R1","LB3"]
           ,["P5","G2","O1","B5","LG5"]
           ,["O2","B3","O3","O0","G0"]
           ,["R0","LB4","R5","LG0","LG3"]
           ]

trs2 = [[{'color': 'O', 'number': '5'}, {'color': 'O', 'number': '4'}], [{'color': 'R', 'number': '2'}, {'color': 'R', 'number': '1'}, {'color': 'R', 'number': '0'}, {'color': 'P', 'number': '6'}, {'color': 'B', 'number': '4'}, {'color': 'B', 'number': '3'}, {'color': 'B', 'number': '2'}], [{'color': 'LB', 'number': '4'}, {'color': 'LB', 'number': '3'}, {'color': 'R', 'number': '3'}, {'color': 'B', 'number': '5'}, {'color': 'B', 'number': '1'}], [{'color': 'O', 'number': '6'}, {'color': 'B', 'number': '0'}, {'color': 'R', 'number': '6'}, {'color': 'R', 'number': '5'}, {'color': 'R', 'number': '4'}], [{'color': 'G', 'number': '6'}, {'color': 'G', 'number': '5'}, {'color': 'G', 'number': '4'}, {'color': 'LB', 'number': '2'}, {'color': 'LB', 'number': '1'}], [{'color': 'O', 'number': '1'}, {'color': 'O', 'number': '0'}, {'color': 'B', 'number': '1'}, {'color': 'LB', 'number': '0'}, {'color': 'P', 'number': '5'}, {'color': 'P', 'number': '4'}], [{'color': 'LB', 'number': '6'}, {'color': 'LB', 'number': '5'}, {'color': 'O', 'number': '3'}, {'color': 'O', 'number': '2'}, {'color': 'P', 'number': '3'}, {'color': 'P', 'number': '2'}], [{'color': 'G', 'number': '3'}, {'color': 'G', 'number': '2'}, {'color': 'G', 'number': '1'}, {'color': 'G', 'number': '0'}, {'color': 'B', 'number': '6'}, {'color': 'P', 'number': '0'}]]

trs3 = [[{'color': 'O', 'number': '5'}, {'color': 'O', 'number': '4'}, {'color': 'O', 'number': '3'}, {'color': 'O', 'number': '2'}],
[{'color': 'R', 'number': '2'}, {'color': 'R', 'number': '1'}, {'color': 'R', 'number': '0'}, {'color': 'P', 'number': '6'}],
[],
[{'color': 'O', 'number': '6'}, {'color': 'B', 'number': '0'}, {'color': 'R', 'number': '6'}, {'color': 'R', 'number': '5'}, {'color': 'R', 'number': '4'}, {'color': 'R', 'number': '3'}],
[{'color': 'G', 'number': '6'}, {'color': 'G', 'number': '5'}, {'color': 'G', 'number': '4'}, {'color': 'LB', 'number': '2'}, {'color': 'LB', 'number': '1'}],
[{'color': 'O', 'number': '1'}, {'color': 'O', 'number': '0'}, {'color': 'B', 'number': '1'}, {'color': 'LB', 'number': '0'}, {'color': 'P', 'number': '5'}, {'color': 'P', 'number': '4'}, {'color': 'P', 'number': '3'}, {'color': 'P', 'number': '2'}, {'color': 'P', 'number': '1'}, {'color': 'P', 'number': '0'}],
[{'color': 'LB', 'number': '6'}, {'color': 'LB', 'number': '5'}, {'color': 'LB', 'number': '4'}, {'color': 'LB', 'number': '3'}],
[{'color': 'G', 'number': '3'}, {'color': 'G', 'number': '2'}, {'color': 'G', 'number': '1'}, {'color': 'G', 'number': '0'}, {'color': 'B', 'number': '6'}, {'color': 'B', 'number': '5'}, {'color': 'B', 'number': '4'}, {'color': 'B', 'number': '3'}, {'color': 'B', 'number': '2'}]]


trs0 = fromStringMatrix(string_trs0)
trs1 = fromStringMatrix(string_trs1)
trs5 = fromStringMatrix(string_trs5)
trs6 = fromStringMatrix(string_trs6)
trs7 = fromStringMatrix(string_trs7)
ultra_btM = fromStringMatrix(ultra_bt)

sols = createGraph(ultra_btM)
progress(ultra_btM, sols)
