library(ggplot2)
AB_test_data <- read.csv("AB_test.csv", header = TRUE)
a_clicks <- AB_test_data[AB_test_data$algorithm == "A", ]$clicks_number
b_clicks <- AB_test_data[AB_test_data$algorithm == "B", ]$clicks_number
result <- t.test(x = b_clicks, y = a_clicks, alternative = "greater", paired = FALSE)
print(result)
cat("Difference in means is", mean(b_clicks) - mean(a_clicks), "\n")
if (result$p.value > 0.05) {
    print("We can't reject null hypothesis (we cant say b is better than a)")
} else {
    print("We have rejected null hypothesis (b is better than a)")
}
# Permutation test
n <- 1000
simple_mean_diff <- abs(mean(a_clicks) - mean(b_clicks))
diffs <- sapply(1:n, function(i) {
    combined <- sample(c(a_clicks, b_clicks))
    new_a <- combined[1:length(a_clicks)]
    new_b <- combined[(length(a_clicks) + 1):length(combined)]
    return(abs(mean(new_a) - mean(new_b)))
})
print(ggplot(as.data.frame(diffs), aes(x = diffs)) +
    geom_histogram(binwidth = 0.5, fill = I("blue")) +
    geom_vline(xintercept = simple_mean_diff, color = "red", size = 1.5) +
    labs(x = "Difference in averages", y = "Count"))
cat("p-value is", length(diffs[diffs >= simple_mean_diff]) / length(diffs), "\n")