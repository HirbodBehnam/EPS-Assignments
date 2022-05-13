data <- read.csv("data.csv")
x <- list(
    data$record_max_temp >= 100,
    data$record_max_temp > 70 & data$record_max_temp < 100,
    data$record_max_temp <= 70
)
y <- list(
    data$record_min_temp >= 40,
    data$record_min_temp < 40 & data$record_min_temp > 20,
    data$record_min_temp <= 20
)
result <- matrix(
    sapply(1:3, function(i) sapply(1:3, function(j)
        sum(y[[i]] & x[[j]], na.rm = TRUE) / nrow(data)
    )), nrow = 3, ncol = 3)
print(result)

cat("y_3 =", sum(result[, 3]), "x_1 =", sum(result[1, ]), "\n")

library("ggplot2")
u <- data$actual_min_temp
v <- data$actual_max_temp
hist(u)
hist(v)
ggplot() + geom_density_2d(aes(x = u, y = v))