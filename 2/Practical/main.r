library("ggplot2")
library("reshape2")
playoff <- function(p, n = 7, trials = 5000) {
    x <- replicate(trials, sum(sample(c(TRUE, FALSE), replace = TRUE, size = n, prob = c(p, 1 - p)), na.rm = TRUE) > n %/% 2)
    return(sum(x, na.rm = TRUE) / trials)
}
# 2
prob <- seq(0.5, 1, by = 0.001)
result <- sapply(prob, function(p) playoff(p))
print(ggplot(data.frame(prob, result), aes(prob, result)) +
    geom_point() +
    geom_smooth() +
    xlab("p") +
    ylab("win probability"))
# 3
numbers <- seq(1, 50, by = 2)
data <- data.frame(numbers)
for (p in seq(0.6, 0.9, by = 0.1)) {
    data[toString(p)] <- sapply(numbers, function(n) playoff(p, n))
}
data <- melt(data, id.vars = 'numbers', variable.name = 'series')
print(ggplot(data, aes(numbers, value)) +
      geom_point(aes(colour = series)) +
      geom_smooth(aes(colour = series)) +
      xlab("rounds") +
      ylab("win probability"))
# 4
playoff <- function(p_home, p_non_home, home_games_n, none_home_games_n, trials = 5000) {
    to_win <- (home_games_n + none_home_games_n) %/% 2
    x <- replicate(trials, {
        result <- c(
            sample(c(TRUE, FALSE), replace = TRUE, size = home_games_n, prob = c(p_home, 1 - p_home)),
            sample(c(TRUE, FALSE), replace = TRUE, size = none_home_games_n, prob = c(p_non_home, 1 - p_non_home))
        )
        return(sum(result, na.rm = TRUE) > to_win)
    })
    return(sum(x, na.rm = TRUE) / trials)
}
# 5
columns <- c("home_win_p", "non_home_win_p", "starts_from_home", "start_matches", "win_p")
data <- data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(data) <- columns
for (home_win_p in seq(0.1, 0.9, by = 0.2)) {
    for (non_home_win_p in seq(0.1, 0.9, by = 0.2)) {
        for (start_matches in seq(3, 15, by = 2)) {
            data[nrow(data) + 1, ] <- c(home_win_p, non_home_win_p, "Yes", start_matches,
                playoff(home_win_p, non_home_win_p, start_matches + 1, start_matches))
            data[nrow(data) + 1, ] <- c(home_win_p, non_home_win_p, "No", start_matches,
                playoff(home_win_p, non_home_win_p, start_matches, start_matches + 1))
        }
    }
}
print(ggplot(data, aes(start_matches, win_p)) +
    geom_point(aes(colour = factor(starts_from_home))) +
    geom_smooth(aes(colour = factor(starts_from_home)), se = FALSE) +
    facet_wrap(vars(home_win_p, non_home_win_p)) +
    labs(x = "first round count", y = "win probability", colour = "Starts from home?"))
# 6
columns <- c("home_win_p", "home_plays_n", "win_p")
data <- data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(data) <- columns
for (home_win_p in seq(0.01, 1, by = 0.01)) {
    non_home_win_p <- home_win_p / 2
    data[nrow(data) + 1, ] <- c(home_win_p, 3,
        playoff(home_win_p, non_home_win_p, 3, 4))
    data[nrow(data) + 1, ] <- c(home_win_p, 4,
        playoff(home_win_p, non_home_win_p, 4, 3))
}
print(ggplot(data, aes(x = as.factor(home_plays_n), y = home_win_p)) +
    geom_raster(aes(fill = win_p)) +
    scale_fill_gradientn(colours = rainbow(5)) +
    labs(x = "Rounds Played in Home", y = "Home Win Probability", colour = "Win Probability"))
# 2-1
lambda <- 1 / 2
data <- data.frame(diff = replicate(1000, {
        # Uncomment these lines for pure poisson random generation
        #x <- rpois(2, lambda)
        #return(abs(x[1] - x[2]))
        return(rexp(1, lambda))
    }))
print(ggplot(data) +
    geom_histogram(aes(x = diff, y = ..density..), binwidth = 1) +
    geom_density(aes(x = diff), fill = "green", alpha = 0.1))
# 2
lambda <- 1 / 2
m <- 1
# Returns P(X > n) where X ~ Exp(lambda)
more_than <- function(lambda, n, trials = 1000) {
    x <- rexp(trials, lambda)
    return(length(x[x > n]) / trials)
}
# Returns P(X > n + m | X > m) where X ~ Exp(lambda)
more_than_if <- function(lambda, n, m, trials = 10000) {
    x <- rexp(trials, lambda)
    return(length(x[x > n + m]) / length(x[x > m]))
}
# Create the data
columns <- c("n", "P(X > n)", "P(X > n + m | X > m)")
data <- data.frame(matrix(nrow = 0, ncol = length(columns)))
colnames(data) <- columns
for (n in seq(0, 4, by = 0.01)) {
    data[nrow(data) + 1, ] <- c(n, more_than(lambda, n), more_than_if(lambda, n, m))
}
data <- melt(data, id.vars = "n", variable.name = "series")
print(ggplot(data, aes(n, value)) +
      geom_point(aes(colour = series)) +
      geom_smooth(aes(colour = series)) +
      xlab("n") +
      ylab("P"))
# 3
n <- 10
lambda <- 1/5
samples <- 1000
data <- data.frame(gamma = rgamma(samples, n, rate = lambda), exp = replicate(samples, sum(rexp(n, rate = lambda))))
print(ggplot(data) +
    stat_density(aes(x = gamma, colour = "green"), geom="line", position="identity") +
    stat_density(aes(x = exp, colour = "red"), geom="line",position="identity"))

