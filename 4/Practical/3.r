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
print(head(simulator1_bias))
print(head(simulator2_bias))