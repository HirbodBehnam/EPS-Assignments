library("ggplot2")
library("reshape2")
# Part 1
simulator1 <- function(data) {
    return(sum((data - mean(data))^2) / (length(data)))
}
simulator2 <- function(data) {
    return(sum((data - mean(data))^2) / (length(data) - 1))
}
true_variance <- 2^2
data <- lapply(2:1000, function(i) rnorm(i, mean = 2, sd = sqrt(true_variance)))
simulator1_bias <- sapply(data, function(i) simulator1(i) - true_variance)
simulator2_bias <- sapply(data, function(i) simulator2(i) - true_variance)
# Part 2
bias_data <- data.frame(n = 2:1000, simulator1_data = simulator1_bias, simulator2_data = simulator2_bias)
bias_data <- melt(bias_data, value.name = "bias", id = "n")
ggplot(bias_data, aes(n, bias, colour = variable)) +
  geom_point() + geom_smooth(colour = "black") + facet_wrap(vars(variable))
# Part 3
data <- lapply(1:10000, function(i) rnorm(10, mean = 2, sd = sqrt(true_variance)))
simulator1_bias <- mean(sapply(data, function(i) simulator1(i) - true_variance))
simulator2_bias <- mean(sapply(data, function(i) simulator2(i) - true_variance))
cat("simulator1 bias is ", simulator1_bias, "\n")
cat("simulator2 bias is ", simulator2_bias, "\n")