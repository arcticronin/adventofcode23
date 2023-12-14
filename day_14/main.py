import numpy as np


def simulation(matrix):
    for k in range(n-1,0,-1):
        for i in range(0,k):
            for j in range(0,m):
                if matrix[i,j]== '.':
                    if matrix[i+1,j] != '#':
                        matrix[i,j] = matrix[i+1,j]
                        matrix[i+1,j] = '.'
    return matrix

def score(matrix):
    n = len(matrix)
    m = len(matrix[0])
    score = 0
    for i in range(n):
        for j in range(m):
            if matrix[i,j]== 'O':
                score += n-i
    return score


with open("input.txt", 'r') as file:
    lines = file.readlines()
    matrix = np.array([list(line.strip()) for line in lines])
    original_matrix = matrix

n = len(matrix)
m = len(matrix[0])


# part 1
print("part 1 result")
print(score(simulation(matrix)))

# part 2
truenumber = 1000000000
matrix = original_matrix

# study patterns, save them in a db
dict = {}
for loopz in range(10):
    key = matrix.tobytes()
    if key in dict:
        dict[key].append(loopz)
    else:
        dict[key] = [loopz]
    if (loopz % 50 == 0):
        print("calculating patterns: ", loopz, "analised")
    simulation(matrix)
    matrix = np.rot90(matrix, -1)

# print dict with max entries in the dictionary
key_with_max_list = max(dict, key=lambda k: len(dict[k]))
print("most frequent item at: ", dict[key_with_max_list])
print("just obtain the item at : (1000000000 - 94) % 84) + 94")
position = ((1000000000 - 94) % 84) + 94
print(position)

matrix = original_matrix
for loopz in range(position):
    if (loopz % 10 == 0):
        print("simulating final result, at : " , loopz , "/", position)
    for rot in range(4):
        matrix = simulation(matrix)
        matrix = np.rot90(matrix, -1) 
print("part 2 result")
print(score(matrix))