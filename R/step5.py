from numba import jit
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
#constants
BLACK = 0
WHITE = 1

#globals
g_matrix = []
fill = []
clean = []

@jit
def process(sr, sc, process_type):
    if(process_type == "3x3"):
        p3x3(sr,sc)
    elif(process_type == "4x3"):
        p4x3(sr,sc)
    elif(process_type == "3x4"):
        p3x4(sr,sc)
    else: print("error in process_type")

"""
kinda gross and manual
all pXxX execute as follows:
1) ensure H1,H2,H3 is fit
2) check corners
3) if corner is white mark neighbords for deletion
4) if neither corner is white check I1,I2,I3
H1 -> I1, H2 -> I3, H3 -> I2
"""
@jit
def p3x3(sr,sc):
    if(g_matrix[sr][sc+1] == BLACK and g_matrix[sr+1][sc] == BLACK
            and g_matrix[sr+2][sc+1] == BLACK and g_matrix[sr+1][sc+2] == BLACK
            and g_matrix[sr+1][sc+1] == WHITE):
        check_corners((sr,sc),(sr,sc+2),(sr+2,sc),(sr+2,sc+2))
    elif:
        check_I()
    else:
        fill.append((g_matrix[sr+1],g_matrix[sc+1]))
@jit
def p4x3(sr,sc):
    if(g_matrix[sr][sc+1] == BLACK and g_matrix[sr+1][sc] == BLACK 
            and g_matrix[sr+2][sc] == BLACK and g_matrix[sr+3][sc+1] == BLACK
            and g_matrix[sr+1][sc+2] == BLACK and g_matrix[sr+2][sc+2] == BLACK
            and g_matrix[sr+1][sc+1] == WHITE and g_matrix[sr+2][sc+1] == WHITE):
        check_corners((sr,sc),(sr+3,sc),(sr+2,sc),(sr+2,sc+3))
    elif:
        check_I()
    else:
        fill.append((g_matrix[sr+1],g_matrix[sc+1]))
        fill.append((g_matrix[sr+2],g_matrix[sc+1]))
@jit
def p3x4(sr,sc):
    if(g_matrix[sr+1][sc] == BLACK and g_matrix[sr][sc+1] == BLACK
            and g_matrix[sr][sc+2] == BLACK and g_matrix[sr+2][sc+1] == BLACK
            and g_matrix[sr+2][sc+2] == BLACK and g_matrix[sr+1][sr+2] == BLACK
            and g_matrix[sr+1][sc+1] == WHITE and g_matrix[sr+1][sc+2] == WHITE):
        check_corners((sr,sc),(sr,sc+3),(sr+2,sc),(sr+2,sc+3))
    elif:
        check_I()
    else:
        fill.append((g_matrix[sr+1],g_matrix[sc+1]))
        fill.append((g_matrix[sr+1],g_matrix[sc+2]))


#receives tuples from pXxX, marks elements for deletion if needed
@jit
def check_corners(tl,tr,br,bl):
    if(g_matrix[tl[0]][tl[1]] == WHITE):
        clean.append((tl[0],tl[1]+1))
        clean.append((tl[0]+1,tl[1]))
    if(g_matrix[tr[0]][tr[1]] == WHITE):
        clean.append((tr[0],tr[1]-1))
        clean.append((tr[0]+1,tr[1]))
    if(g_matrix[br[0]][br[1]] == WHITE):
        clean.append((br[0]-1,br[1]))
        clean.append((br[0],br[1]-1))
    if(g_matrix[bl[0]][bl[1]] == WHITE):
        clean.append((bl[0]-1,bl[1]))
        clean.append((bl[0],bl[1]+1))
@jit
def clean_marked():
    for i in clean:
        g_matrix[i[0]][i[1]] = WHITE
@jit
def fill_marked():
    for i in fill:
        g_matrix[i[0]][i[1]] = BLACK

# iterates, drives processes steps 1-5
#15:21 23-8, if issues remove - 3
@jit
def clean_s5(matrix):
    g_matrix = matrix
    for row in range(0, len(matrix)-3):
        for col in range(0, len(matrix[0])-3):
            if(row+4 < len(matrix) and col+4 < len(matrix)):
                process(row, col, "3x3")
                process(row, col, "4x3")
                process(row, col, "3x4")
            else:
                if(row+4 < len(matrix)):
                    process(row,col,"4x3")
                if(col+4 < len(matrix)):
                    process(row,col,"3x4")
                #process 3x3 anyway
                process(row,col,"3x3")
    clean_marked()
    fill_marked()
    #TODO: else if on step 2, step 6 onward

