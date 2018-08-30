from numba import jit
import pandas as pd
import csv
"""
naive implementation of steps 1-5

i'm anticipating even with jit poor
relative performance in comparison to
an algorithm designed for large scale iteration
but this can serve as clear, readable psuedo code
as well as identify any issues with a linear approach

ben escobar, csafe-isu 2k24

src:
    IEEE TRANSACTIONS ON SYSTEMS, MAN, 
    AND CYBERNETICS, VOL. SMC - 13, 
    NO. 1, JANUARY/FEBRUARY 1983 

!!! recall all programatic dimensions will be Row x Col, not X x Y
"""
# constants
BLACK = 0
WHITE = 1
DNC = 3  # "do not care"
"""
# globals
g_matrix = []
fill = []
clean = []
"""
# probably should be a regex tostring ?? idk
I1MASK = [
    [DNC, DNC, BLACK, BLACK, BLACK, DNC, DNC],
    [DNC, BLACK, BLACK, BLACK, BLACK, BLACK, DNC],
    [DNC, BLACK, BLACK, BLACK, BLACK, BLACK, DNC],
    [BLACK, BLACK, BLACK, WHITE, BLACK, BLACK, BLACK],
    [DNC, BLACK, BLACK, BLACK, BLACK, BLACK, DNC],
    [DNC, BLACK, BLACK, BLACK, BLACK, BLACK, DNC],
    [DNC, DNC, BLACK, BLACK, BLACK, DNC, DNC]
]
I2MASK = [
    [DNC, DNC, BLACK, BLACK, BLACK, DNC, DNC],
    [DNC, BLACK, BLACK, BLACK, BLACK, BLACK, DNC],
    [DNC, BLACK, BLACK, BLACK, BLACK, BLACK, DNC],
    [BLACK, BLACK, BLACK, WHITE, BLACK, BLACK, BLACK],
    [BLACK, BLACK, BLACK, WHITE, BLACK, BLACK, BLACK],
    [DNC, BLACK, BLACK, BLACK, BLACK, BLACK, DNC],
    [DNC, BLACK, BLACK, BLACK, BLACK, BLACK, DNC],
    [DNC, DNC, BLACK, BLACK, BLACK, DNC, DNC]
]
I3MASK = [
   [DNC,DNC,BLACK,BLACK,BLACK,BLACK,DNC,DNC],
   [DNC,DNC,BLACK,BLACK,BLACK,BLACK,DNC,DNC],
   [DNC,BLACK,BLACK,BLACK,BLACK,BLACK,BLACK,DNC],
   [BLACK,BLACK,BLACK,WHITE,WHITE,BLACK,BLACK,BLACK],
   [DNC,BLACK,BLACK,BLACK,BLACK,BLACK,BLACK,DNC],
   [DNC,DNC,BLACK,BLACK,BLACK,BLACK,DNC,DNC],
   [DNC,DNC,BLACK,BLACK,BLACK,BLACK,DNC,DNC],
]


# iterates through consant masks defined above
# compares current


#@jit
def compareMask(mask, sr, sc, matrix):
    #print('comparing masks')
    sc_copy = sc
    for i in range(len(mask)):
        for j in range(len(mask[0])):
            #print("indexes in compare mask: ", i, j)
            #print("sr sc in compare mask: ", sr, sc)
            if (sr >= len(matrix) or sc >= len(matrix[0]) or sr < 0 or sc < 0):
                return False
            #print('about to assign value? in comparemask')
            value = matrix[sr][sc]
            if (not value == mask[i][j] and not mask[i][j] == DNC):
                return False
            sc = sc + 1
        sr = sr + 1
        sc = sc_copy
    return True


#@jit
def process(sr, sc, process_type, fill, clean, matrix):
    if (process_type == "3x3"):
        #print('processing 3x3"')
        p3x3(sr, sc, fill, clean, matrix)
    elif (process_type == "4x3"):
        #print('processing 4x3"')
        p4x3(sr, sc, fill, clean, matrix)
    elif (process_type == "3x4"):
        #print('processing 4x3"')
        p3x4(sr, sc, fill, clean, matrix)
    else:
        print("error in process_type")


"""
kinda gross and manual
all pXxX execute as follows:
1) ensure H1,H2,H3 is fit
2) check corners
3) if corner is white mark neighbords for deletion
4) if neither corner is white check I1,I2,I3
H1 -> I1, H2 -> I3, H3 -> I2
"""


# !! idea 28 8 18, define constant masks, splice og matrix to check conditional
# should be less messy and ideally faster


#@jit
# ensure before I masks are checked the sr and sc are changed in these functions below
def p3x3(sr, sc, fill, clean, matrix):
    # print("length of g_matrix in p3x3",len(matrix))
    # print(g_matrix[sr][sc+1])
    if (matrix[sr][sc + 1] == BLACK and matrix[sr + 1][sc] == BLACK
            and matrix[sr + 2][sc + 1] == BLACK and matrix[sr + 1][sc + 2] == BLACK
            and matrix[sr + 1][sc + 1] == WHITE):
        if(check_corners((sr, sc), (sr, sc + 2), (sr + 2, sc + 2), (sr + 2, sc), clean, matrix)):
            return
        elif(compareMask(I1MASK, sr - 2, sc - 2, matrix)):
            return
        else:
            fill.append((sr + 1, sc + 1))

#@jit
def p4x3(sr, sc, fill, clean, matrix):
    #print('inside p4x3')
    if (matrix[sr][sc + 1] == BLACK and matrix[sr + 1][sc] == BLACK
            and matrix[sr + 2][sc] == BLACK and matrix[sr + 3][sc + 1] == BLACK
            and matrix[sr + 1][sc + 2] == BLACK and matrix[sr + 2][sc + 2] == BLACK
            and matrix[sr + 1][sc + 1] == WHITE and matrix[sr + 2][sc + 1] == WHITE):
        if(check_corners((sr, sc), (sr, sc + 2), (sr + 3, sc+2), (sr + 3, sc), clean, matrix)):
            return
        elif(compareMask(I2MASK, sr - 2, sc - 2, matrix)):
            return
        else:
            fill.append((sr + 1, sc + 1))
            fill.append((sr + 2, sc + 1))

#@jit
def p3x4(sr, sc, fill, clean, matrix):
    # debug = [matrix[sr+1][sc],matrix[sr][sc+1],matrix[sr][sc+2],matrix[sr+2][sc+1],
    #          matrix[sr+2][sc+2],matrix[sr+1][sr+3],matrix[sr+1][sc+1],matrix[sr+1][sc+2]]
    if (matrix[sr + 1][sc] == BLACK and matrix[sr][sc + 1] == BLACK
            and matrix[sr][sc + 2] == BLACK and matrix[sr + 2][sc + 1] == BLACK
            and matrix[sr + 2][sc + 2] == BLACK and matrix[sr + 1][sc + 3] == BLACK
            and matrix[sr + 1][sc + 1] == WHITE and matrix[sr + 1][sc + 2] == WHITE):
        if(check_corners((sr, sc), (sr, sc + 3), (sr + 2, sc + 3), (sr + 2, sc), clean, matrix)):
            return
        elif(compareMask(I3MASK, sr - 2, sc - 2, matrix)):
            return
        else:
            fill.append((sr + 1, sc + 1))
            fill.append((sr + 1, sc + 2))


# receives tuples from pXxX, marks elements for deletion if needed
#@jit
def check_corners(tl, tr, br, bl, clean, matrix):
    #print("checking corners..")
    prev_clean_len = len(clean)
    if (matrix[tl[0]][tl[1]] == WHITE):
        print('appending to clean')
        clean.append((tl[0], tl[1] + 1))
        clean.append((tl[0] + 1, tl[1]))
    if (matrix[tr[0]][tr[1]] == WHITE):
        print('appending to clean')
        clean.append((tr[0], tr[1] - 1))
        clean.append((tr[0] + 1, tr[1]))
    if (matrix[br[0]][br[1]] == WHITE):
        print('appending to clean')
        clean.append((br[0] - 1, br[1]))
        clean.append((br[0], br[1] - 1))
    if (matrix[bl[0]][bl[1]] == WHITE):
        clean.append((bl[0] - 1, bl[1]))
        clean.append((bl[0], bl[1] + 1))
    return prev_clean_len != len(clean)

#so i'd ideally like not to mutate the values and instead return where it should be replaced?
#@jit
def clean_marked(clean, matrix):
    for i in clean:
        if(i[0] < 0 or i[1] < 0):
            continue
        print("to clean (row,col): ",i[0],i[1])
        #matrix[i[0]][i[1]] = WHITE


#@jit
def fill_marked(fill, matrix):
    for i in fill:
        if(i[0] < 0 or i[1] < 0):
            continue
        print("to fill (row,col): ",i[0],i[1])
        #matrix[i[0]][i[1]] = BLACK


# iterates, drives processes steps 1-5
# main driver function
# 15:21 23-8, if issues remove - 3

# improvements in logic can be made below most likely
#@jit
def clean_s5(matrix):
    # matrix = []
    fill = []
    clean_list(matrix)
    clean = []
    #p3x4(7,20,fill,clean,matrix)
    #print("len of gmatrix in local cleans5: ", len(matrix))
    for row in range(0, len(matrix) - 4):
        for col in range(0, len(matrix[0]) - 4):
            #print("row col of cur iteration:", row, col)
            if (row + 4 < len(matrix)):
                process(row, col, "4x3", fill, clean, matrix)
            if (col + 4 < len(matrix[0])):
                process(row, col, "3x4", fill, clean, matrix)
            if (row + 3 < len(matrix) and col + 3 < len(matrix[0])):
                process(row, col, "3x3", fill, clean, matrix)
    clean_marked(clean, matrix)
    fill_marked(fill, matrix)
    print("success, no errors (but maybe undefined behavior)")
sampleMatrix = [
    #0 1 2 3 4 5 6 7 8 9 1 2 3 4 5 6 7 8 9 1 2
    [1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1], #0
    [0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1], #1
    [1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1], #2
    [1,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1], #3
    [1,1,1,1,1,1,0,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1], #4
    [1,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1], #5
    [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1], #6
    [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1], #7
    [1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1], #8
    [1,1,1,1,1,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1], #9
    [1,1,1,1,1,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
    [1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
    [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
    [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
]
#verified all H masks working
sampleMatrix1 = [
    #0 1 2 3 4 5 6 7 8 9 1 1 2 3 4 5 6 7 8 9 2
    [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1], #0
    [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1], #1
    [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1], #2
    [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1], #3
    [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1], #4
    [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1], #5
    [1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,1,1,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1], #6
    [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1], #7
    [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1], #8
    [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1], #9
    [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
    [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
    [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
    [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
]

"""
    [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1], #0
    [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1], #1
    [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1], #2
    [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1], #3
    [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1], #4
    [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1], #5
    [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1], #6
    [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1], #7
    [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1], #8
    [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1], #9
    [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
    [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
    [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
    [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
"""
r_list = pd.read_csv("./sample_c_all.csv")
def clean_list(r_list):
    for i in r_list:
        i.pop(0)
#print(r_list.values.tolist())
clean_s5(r_list.values.tolist())