library("ggplot2")
fish <- function() {
    return(mean(sample((1:5) * 10, 45, replace = TRUE, prob = c(0.4, 0.2, 0.2, 0, 0.2))))
}
print(mean((replicate(10000, fish()))))
print(var((replicate(10000, fish()))))