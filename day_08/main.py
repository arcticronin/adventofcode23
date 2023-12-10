def walk(d, node, direction):
        if direction == 'L':
            return d[node][0]
        elif direction == 'R':
            return d[node][1]

def check_Z( l ):
    for n in l:
        if n[-1]!="Z":
            return False
    return True

## parsing
file1 = open('input.txt', 'r')
Lines = file1.readlines()

d = dict()
pattern = Lines[0].strip()

for line in Lines[2:]:
    s = line.strip()
    parsed = s.replace(" ", "").replace("(","").replace(")","").replace("=",",").split(",")
    key, l , r = parsed
    d[key] =(l,r)

## part 1
position = "AAA"
steps = 0 
walkingDir = pattern
while position != "ZZZ":
    if walkingDir == '':
        walkingDir = pattern
    position = walk(d,position,walkingDir[0])
    walkingDir = walkingDir[1:]
    steps += 1 
print(steps)

# part 2
positionList = []
for node in d.keys():
    if node[2] == "A":
        positionList.append(node)
print(positionList)

## that is too long
## splicing it and trying a better idea

## results analysing first2, first3 and last3
positionList = positionList[:3]
steps2 = 0
walkingDir = pattern
while (not check_Z(positionList)):
    if walkingDir == '' :  
        walkingDir = pattern
    for i in range(len(positionList)):
        positionList[i] = walk(d , positionList[i],walkingDir[0])
    walkingDir = walkingDir[1:]
    steps2 += 1 

print(steps2)
## results analysing first2, first3 and last3
primi_2 = 922657
primi_3 = 67353961
ultimi_3 = 96962783

from numpy import lcm

print(lcm(primi_3, ultimi_3))