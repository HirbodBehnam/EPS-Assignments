r <- 3
w <- 6
g <- 9
e <- (r + w + g) / 3
trials <- 10000
gen <- function(z, y, x) sample(c(rep(0, x), rep(1, y), rep(2, z)))
trials_result <- sapply(1:trials, function(x) {
    boxes <- gen(r, w, g)
    samples <- sapply(
        seq(1, length(boxes), 3),
        function(i) boxes[i] == boxes[i + 1] && boxes[i + 1] == boxes[i + 2]
    )
    return(length(samples[samples == TRUE]))
})
print(sum(trials_result) / trials)
print(e * (choose(r, 3) + choose(w, 3) + choose(g, 3)) / choose(3 * e, 3))