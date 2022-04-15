import numpy as np
from numpy.random import default_rng
rng = default_rng()
LAMBDA = 10
ROUNDS = 10000
sum = 0
for _ in range(ROUNDS):
    sum += np.min(rng.exponential(1 /LAMBDA, 2))
print(sum / ROUNDS)