import pandas as pd
def compareBinaries(matrix1,matrix2):
    difs = []
    if(len(matrix1)!=len(matrix2) or len(matrix1[0])!=len(matrix2[0])):
        print("issue with lengths, aborting")
        return
    for row in range(matrix1):
        for col in range(matrix1[0]):
            if(matrix1[row][col]!=matrix2[row][col]):
                difs.append((row,col))
    return pd.concat(difs)
