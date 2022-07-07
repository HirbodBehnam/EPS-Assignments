library(ggplot2)
options(repr.plot.width=15, repr.plot.height=8)
salaries <- read.csv("datasets/Salaries.csv")
print(ggplot(salaries, aes(yrs.service, salary)) +
    geom_point(aes(shape = sex, color = rank)) +
    xlab("years of service"))
print(ggplot(salaries, aes(yrs.service, salary)) +
    geom_point(aes(fill = rank), shape = 21, color = "white", size = 1, stroke = 1) +
    xlab("years of service"))