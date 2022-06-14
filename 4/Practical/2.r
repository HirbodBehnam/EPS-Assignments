library(dplyr)
library(ggplot2)
vaccination_data <- read.csv("vaccination.csv", header = TRUE)
# https://stackoverflow.com/a/67826564/4213397
# https://ggplot2.tidyverse.org/reference/geom_bar.html
# https://www.marsja.se/r-add-column-to-dataframe-based-on-other-columns-conditions-dplyr/
classifier <- function(age) {
    if (12 <= age && age < 18) {
        return("teenage")
    }
    if (18 <= age && age < 40) {
        return("young")
    }
    if (40 <= age && age < 70) {
        return("adult")
    }
    return("old")
}
vaccination_data <- vaccination_data %>%
    mutate(age_group = sapply(age, function(i) classifier(i)))
print(ggplot(vaccination_data, aes(age_group)) +
    geom_bar(aes(fill = vaccine_type)))
print(ggplot(vaccination_data, aes(vaccine_type)) +
    geom_bar(aes(fill = vaccine_type)) + facet_wrap(vars(age_group)))
observations <- sapply(sort(unique(vaccination_data$age_group)), function(age_g) {
    return(sapply(sort(unique(vaccination_data$vaccine_type)), function(vac_type) {
        return(nrow(vaccination_data[vaccination_data$vaccine_type == vac_type & vaccination_data$age_group == age_g,]))
    }))
})
print(observations)
# We use chi-test
observations <- matrix(observations, ncol = length(unique(vaccination_data$age_group)))
row_margin <- rowSums(observations)
column_margin <- colSums(observations)
total <- sum(observations)
expected <- observations
for (i in 1:nrow(observations)) {
    for (j in 1:ncol(observations)) {
        expected[i, j] <- row_margin[i] * column_margin[j] / total
    }
}
stat <- sum((observations - expected)^2 / expected)
p_value <- 1 - pchisq(stat, (nrow(observations) - 1) * (ncol(observations) - 1))
cat("p value of chi-test is", p_value, "\n")
if (p.value > 0.05) {
    print("We can't reject null hypothesis")
} else {
    print("We have rejected null hypothesis (they are dependant)")
}