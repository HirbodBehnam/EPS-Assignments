library("ggplot2")
library("orderstats")
library("reshape2")
data <- replicate(10000, sort(runif(15, min = 0, max = 1)), simplify = FALSE)
data_frame <- data.frame(
    a = unlist(lapply(data, function(ary) ary[1])),
    b = unlist(lapply(data, function(ary) ary[10])),
    c = unlist(lapply(data, function(ary) ary[15])),
    orderstats_a = order_probs(10000, 1, 15),
    orderstats_b = order_probs(10000, 10, 15),
    orderstats_c = order_probs(10000, 15, 15)
)
data_frame <- melt(data_frame)
# order_probs is defined as rbeta(draw_size, k, n + 1 - k)
# From https://rdrr.io/cran/orderstats/src/R/order_stats.R
ggplot(data_frame, aes(value, colour = variable)) +
    geom_density() +
    xlab("x")

# Mostly from https://gist.github.com/arai17/fd3f481e6c2321b777aa0124a4deaf8a
data_frame <- data.frame(
    x = 1:15,
    y = sapply(1:15, function(i) mean(unlist(
        lapply(data, function(ary) ary[i])
    ))),
    yend = rep(0, 15)
)
ggplot(data_frame, aes(x = x, y = y, xend = x, yend = yend)) +
    geom_point() +
    geom_smooth() +
    scale_x_continuous(name = "Index of element",
        breaks = 1:15) +
    scale_y_continuous(name = "Mean")