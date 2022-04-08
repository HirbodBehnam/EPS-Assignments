import itertools
import math

n = 7
k = 6
numbers = list(range(1, n + 1))
count = 0
for permute in itertools.permutations(numbers):
    ok = True
    for i in permute[:permute.index(k)]:
        if i > k:
            ok = False
            break
    if ok:
        count += 1
print(count)
# My formula
count = 0
for i in range(1, k + 1):
    count += math.comb(k - 1, i - 1) * math.factorial(i - 1) * math.factorial(n - i)
print(count)