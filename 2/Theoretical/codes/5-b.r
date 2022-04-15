playrounds <- function(rounds) 10 * length(which(sample(1:24, rounds, replace = TRUE) <= 5))
print(var(sapply(1:10000, function(i) playrounds(10))))