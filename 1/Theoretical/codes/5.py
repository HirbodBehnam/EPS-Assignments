import random
from math import comb
R = 4
W = 5
G = 3
E = (R + W + G) // 3
TRIALS = 10000
found = 0
for _ in range(TRIALS):
    boxes = ([0] * R) + ([1] * W) + ([2] * G)
    random.shuffle(boxes)
    for i in range(0, len(boxes), 3):
        if boxes[i] == boxes[i + 1] and boxes[i + 1] == boxes[i + 2]:
            found += 1

print(found / TRIALS)
print(E * (comb(R, 3) + comb(W, 3) + comb(G, 3)) / comb(3 * E, 3))