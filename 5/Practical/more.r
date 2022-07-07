library(ggplot2)
library(dplyr)
library(stringr)
buyers <- read.csv("datasets/bike_buyers_na.csv")
print(head(buyers, n = 10))
print(summary(buyers))
# https://stackoverflow.com/a/53346070/4213397
buyers <- buyers %>% mutate_at(vars(Cars), ~ifelse(is.na(.x), median(.x, na.rm = TRUE), .x))
buyers <- buyers %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))
print(summary(buyers))
# I'll remove all empty ones
# I can also use  %>% mutate_at(vars(Region), ~ifelse(.x == "", "NA", .x))
# to create a bar for them as well
buyers_temp <- buyers[buyers$Region != "",]
print(ggplot(buyers_temp, aes(Region)) + geom_bar())
# Just like salaries we can find out income to age and gender
print(ggplot(buyers, aes(Age, Income)) +
    geom_point(aes(color = Gender)))
# Just a chart for Age
ggplot(buyers, aes(x = Age)) + geom_histogram(colour = "black", fill = "white")
# Just like above!
# http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
ggplot(buyers, aes(x = Age)) +
    geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
    geom_density(alpha = .2, fill = "red")