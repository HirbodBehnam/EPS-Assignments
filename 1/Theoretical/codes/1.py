import itertools
import math

for to in range(1, 10):
	numbers = list(range(1, to + 1))
	ok = 0
	for subset in itertools.permutations(numbers):
		sum = 0
		ok_subset = True
		for i in subset:
			sum += i
			if sum % 3 == 0:
				ok_subset = False
				break
		if ok_subset:
			print(subset)
			ok += 1
	print(f"{to}: {ok / math.factorial(to)}")