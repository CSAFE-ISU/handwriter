#!/usr/bin/env pypy3
#Do not remove above line, results in poor performance!
import sys
import numpy as np
import pandas as pd
import time
sys.path.append("/home/esc/git_repos/fall_18/work/handwriter/R")
from maskconstants import *
"""
Preprocessing of a binary image
20 Sept 18: This preproccesing variant is expected to be slower.
This intends on returning the original binary matrix after intensive mutation.
ben escobar, csafe-isu 
src:
    IEEE TRANSACTIONS ON SYSTEMS, MAN, 
    AND CYBERNETICS, VOL. SMC - 13, 
    NO. 1, JANUARY/FEBRUARY 1983 
    
4 Sept 18:
    All steps except step 6 have been implemented.
    Steps 1-5 have been tested.
    Please see attatched "benlog" text document for a comprehensive log
"""
def compareBinaries(matrix1,matrix2):
    difs = []
    if(len(matrix1)!=len(matrix2) or len(matrix1[0])!=len(matrix2[0])):
        print("issue with lengths, aborting")
        return
    for row in range(len(matrix1)):
        for col in range(len(matrix1[0])):
            if(matrix1[row][col]!=matrix2[row][col]):
                difs.append([row,col])
    difs = pd.DataFrame(difs,columns=['row','col'])
    return difs

def compareBinariesDif(matrixOg,matrixNew):
    difs = []
    if(len(matrixOg)!=len(matrixNew) or len(matrixOg[0])!=len(matrixNew[0])):
        print("issue with lengths, aborting")
        return
    for row in range(len(matrixOg)):
        for col in range(len(matrixOg[0])):
            if(matrixOg[row][col] == BLACK and matrixNew[row][col] != BLACK):
                difs.append([row,col,"clean"])
            elif(matrixOg[row][col] == WHITE and matrixNew[row][col] != WHITE):
                difs.append([row,col,"fill"])
    difs = pd.DataFrame(difs,columns=['row','col','type'])
    return difs

def compareMask(mask, sr, sc, matrix):
    """ Compares a constant mask to the current point in the vector, if possible
    :param mask: 2D List filled with constants representing pixels Black, White, or Either
    :param sr: Starting row to check the mask
    :param sc: Starting col to check the mask
    :param matrix: Binary representation of handwriting sample
    :return: If the mask was able to be matched given sr and sc
    """

    sc_copy = sc
    for i in range(len(mask)):
        for j in range(len(mask[0])):
            if (sr >= len(matrix) or sc >= len(matrix[0]) or sr < 0 or sc < 0):
                return False
            value = matrix[sr][sc]
            """
            print(value, "value")
            print(mask, "og mask")
            print(mask[i], "mask at i")
            print(mask[i][j], "mask at i,j")
            """
            if (not (value == mask[i][j]) and not (mask[i][j] == DNC)):
                return False
            sc = sc + 1
        sr = sr + 1
        sc = sc_copy
    return True

def process(sr, sc, process_type, fill, clean, matrix):
    """
    Processes binary image's corners for each initial H mask match
    :param sr: Starting row to check the mask
    :param sc: Starting col to check the mask
    :param process_type: Dimension of H mask checked
    :param fill: List of tuples (row,col) of elements to be filled
    :param clean: List of tuples (row,col) of elements to be cleaned
    :param matrix: Binary representation of handwriting sample
    :return: None
    """
    if (process_type == "3x3" and matrix[sr+1][sc+1] == WHITE):
        #print('processing 3x3"')
        p3x3(sr, sc, fill, clean, matrix)
    elif (process_type == "4x3" and matrix[sr+1][sc+1] == WHITE and matrix[sr+2][sc+1] == WHITE):
        #print('processing 4x3"')
        p4x3(sr, sc, fill, clean, matrix)
    elif (process_type == "3x4" and matrix[sr+1][sc+1] == WHITE and matrix[sr+1][sc+2] == WHITE):
        #print('processing 4x3"')
        p3x4(sr, sc, fill, clean, matrix)
    else:
        a=1
        #print("nothing processed")

def p3x3(sr, sc, fill, clean, matrix):
    """
    Processes binary image with H1 mask
    :param sr: Starting row to check the mask
    :param sc: Starting col to check the mask
    :param fill: List of tuples (row,col) of elements to be filled
    :param clean: List of tuples (row,col) of elements to be cleaned
    :param matrix: Binary representation of handwriting sample
    :return: None
    """
    if (matrix[sr][sc + 1] == BLACK and matrix[sr + 1][sc] == BLACK
            and matrix[sr + 2][sc + 1] == BLACK and matrix[sr + 1][sc + 2] == BLACK
            and matrix[sr + 1][sc + 1] == WHITE):
        if(check_corners((sr, sc), (sr, sc + 2), (sr + 2, sc + 2), (sr + 2, sc), clean, matrix)):
            return
        elif(compareMask(I1MASK, sr - 2, sc - 2, matrix)):
            return
        else:
            #print("APPENDING FILL")
            fill.append((sr + 1, sc + 1))

def p4x3(sr, sc, fill, clean, matrix):
    """
    Processes binary image with H3 mask
    :param sr: Starting row to check the mask
    :param sc: Starting col to check the mask
    :param fill: List of tuples (row,col) of elements to be filled
    :param clean: List of tuples (row,col) of elements to be cleaned
    :param matrix: Binary representation of handwriting sample
    :return: None
    """
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

def p3x4(sr, sc, fill, clean, matrix):
    """
    Processes binary image with H2 mask
    :param sr: Starting row to check the mask
    :param sc: Starting col to check the mask
    :param fill: List of tuples (row,col) of elements to be filled
    :param clean: List of tuples (row,col) of elements to be cleaned
    :param matrix: Binary representation of handwriting sample
    :return: None
    """
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


def check_corners(tl, tr, br, bl, clean, matrix):
    """
    Checks corners to delete neighbors if a white cell is found, step 2
    :param tl: Neighboring tuple (row,col) top left of cell under analysis
    :param tr: Neighboring tuple (row,col) top right of cell under analysis
    :param br: Neighboring tuple (row,col) bottom right of cell under analysis
    :param bl: Neighboring tuple (row,col) bottom left of cell under analysis
    :param clean: List of tuples (row,col) of elements to be cleaned
    :param matrix: Binary representation of handwriting sample
    :return:
    """
    #TODO:
    #9 oct make all else ifs
    prev_clean_len = len(clean)
    if (matrix[tl[0]][tl[1]] == WHITE):
        #print('appending to clean')
        clean.append((tl[0], tl[1] + 1))
        clean.append((tl[0] + 1, tl[1]))
    if (matrix[tr[0]][tr[1]] == WHITE):
        #print('appending to clean')
        clean.append((tr[0], tr[1] - 1))
        clean.append((tr[0] + 1, tr[1]))
    if (matrix[br[0]][br[1]] == WHITE):
        #print('appending to clean')
        clean.append((br[0] - 1, br[1]))
        clean.append((br[0], br[1] - 1))
    if (matrix[bl[0]][bl[1]] == WHITE):
        #print('appending to clean')
        clean.append((bl[0] - 1, bl[1]))
        clean.append((bl[0], bl[1] + 1))
    return prev_clean_len != len(clean)

def clean_marked(clean, matrix):
    """
    Clean all marked holes
    4 Sep 18: Currently only prints
    :param clean: List of tuples (row,col) of elements to be cleaned
    :param matrix: Binary representation of handwriting sample
    :return:
    """
    for i in clean:
        if(i[0] < 0 or i[1] < 0):
            continue
        #print("to clean (row,col): ",i[0],i[1])
        matrix[i[0]][i[1]] = WHITE


def fill_marked(fill, matrix):
    """
    Fill all marked holes
    4 Sep 18: Currently only prints
    :param clean: List of tuples (row,col) of elements to be cleaned
    :param matrix: Binary representation of handwriting sample
    :return:
    """
    for i in fill:
        if(i[0] < 0 or i[1] < 0):
            continue
        #print("to fill (row,col): ",i[0],i[1])
        matrix[i[0]][i[1]] = BLACK

def compareMasks(masks,sr,sc,matrix):
    """ Compares constant masks to the current point in the vector, if possible
    :param masks: 2D Lists filled with constants representing pixels Black, White, or Either
    :param sr: Starting row to check the masks
    :param sc: Starting col to check the masks
    :param matrix: Binary representation of handwriting sample
    :return: If the masks were able to be matched given sr and sc
    """
    for mask in masks:
        if compareMask(mask,sr,sc,matrix):
            return True
    return False
def s_11(matrix,fill,clean):
    #if(len(clean)>0):
    #clean = []
    for row in range(0,len(matrix)-4):
        for col in range(0,len(matrix[0])-4):
            #NEW CONDITIONAL
            if((matrix[row+1][col+2] == WHITE and matrix[row][col+2] == WHITE and matrix[row+2][col+2] == BLACK)
                    or (matrix[row+3][col+2] == WHITE and matrix[row+4][col+2] == WHITE and matrix[row+3][col+2] == BLACK)):
                if compareMasks(DU3_MASKS,row,col,matrix):
                    clean.append((row+2,col+2))
    clean_marked(clean,matrix)
    return clean

def s7_10(matrix,fill,clean,s12 = False):
    """
    Execute steps 7 through ten of cited paper
    :param matrix: Binary representation of handwriting sample
    :param fill:  List of tuples (row,col) of elements to be filled
    :param clean: List of tuples (row,col) of elements to be cleaned
    :return: None
    """
    to_compare = DU_MASKS
    if(s12 == True):
        to_compare = DU1_MASKS
    for row in range(0,len(matrix)-4):
        for col in range(0,len(matrix[0])-4):
            #this if conditional is very very new
            if((matrix[row+1][col+2] == WHITE and matrix[row][col+2] == WHITE and matrix[row+2][col+2] == BLACK)
                    or (matrix[row+3][col+2] == WHITE and matrix[row+4][col+2] == WHITE and matrix[row+3][col+2] == BLACK)):
                if compareMasks(to_compare,row,col,matrix):
                    clean.append((row+2,col+2))
    clean_marked(clean,matrix)
    return clean
    #! if clean_marked behavior is changed this line must change

"""

"""
def s6(matrix,fill,clean):
    #script was complaining so tightened bounds
    for row in range(1,len(matrix)-1):
        for col in range(1,len(matrix[0])-1):
            if(matrix[row][col]==BLACK):
                s6check(matrix,row,col,clean)
    clean_marked(clean,matrix)

def s6check(matrix,row,col,clean):
    neighbors = []
    black_neighbors = 0
    connectivity = 0
    neighbors.append(matrix[row - 1][col - 1])
    neighbors.append(matrix[row - 1][col])
    neighbors.append(matrix[row - 1][col + 1])
    neighbors.append(matrix[row][col + 1])
    neighbors.append(matrix[row + 1][col + 1])
    neighbors.append(matrix[row + 1][col])
    neighbors.append(matrix[row + 1][col - 1])
    neighbors.append(matrix[row][col - 1])
    neighbors.append(matrix[row - 1][col - 1])
    #counting neighbors -2 from length

    #in case having issues
    for x in range(0,len(neighbors)-1):
        if(neighbors[x]== WHITE and neighbors[x+1] == BLACK):
            connectivity += 1
        if(neighbors[x] == BLACK):
            black_neighbors += 1

    #print("connectivity: ",connectivity,"black neighbors: ",black_neighbors)
    if(black_neighbors < 3 and connectivity < 2):
        print("connectivity: ",connectivity,"black neighbors: ",black_neighbors)
        clean.append((row,col))

# improvements in logic can be made below most likely
def s1_5(matrix,fill,clean):
    for row in range(0, len(matrix) - 3):
        for col in range(0, len(matrix[0]) - 3):
            #print("row col of cur iteration:", row, col)
            if (row + 4 < len(matrix)):
                process(row, col, "4x3", fill, clean, matrix)
            if (col + 4 < len(matrix[0])):
                process(row, col, "3x4", fill, clean, matrix)
            #if (row + 3 < len(matrix) and col + 3 < len(matrix[0])):
            process(row, col, "3x3", fill, clean, matrix)
    clean_marked(clean, matrix)
    fill_marked(fill, matrix)
    #return [fill,clean]

#WARNING, should behave odd as I added return statements to the sX_X(...) to put dots on the image in R
def preprocess(matrix):
    """
    Preprocess a binary image
    :param matrix: Binary representation of handwriting sample
    :param fill:  List of tuples (row,col) of elements to be filled
    :param clean: List of tuples (row,col) of elements to be cleaned
    :return: matrix: mutated result of matrix
    """
    matrix = np.copy(matrix)
    matrix.flags.writeable = True
    clean = []
    fill = []
    s1 = time.time()
    s1_5(matrix,fill,clean)
    s1e = time.time()
    print("steps 1-5 time: ",s1-s1e)
    s2 = time.time()
    s6(matrix,fill,clean)
    s2e = time.time()
    print("step 6 time: ",s2-s2e)
    clean = []
    s3 = time.time()
    if(len(s7_10(matrix,fill,clean))>0):
        s3e = time.time()
        print("steps 7-10 time: ",s3-s3e)
        clean = []
        s4 = time.time()
        if(len(s_11(matrix,fill,clean))>0):
            clean = []
            s7_10(matrix,fill,clean,True)
        s4e = time.time()
        print("step 11 time: ",s4-s4e)
    print("success, no errors (but maybe undefined behavior)")
    return matrix

#okay now that i've read some numpy documentation this seems like it'll work fine
"""
def preprocess_s1_5(matrix):
    
    Preprocess a binary image, duplicating code for presentation purposes.. will remove
    :param matrix: Binary representation of handwriting sample
    :param fill:  List of tuples (row,col) of elements to be filled
    :param clean: List of tuples (row,col) of elements to be cleaned
    :return: None
    
    clean = []
    fill = []
    rt = s1_5(matrix,fill,clean)
    print(rt)
    return rt

def preprocess_s7_10(matrix):
    print(matrix)
    matrix = np.copy(matrix)
    matrix.flags.writeable = True
    clean = []
    fill = []
    rt = s7_10(matrix,fill,clean)
    return rt
"""
