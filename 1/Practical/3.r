# radius = 1, Equilateral triangle side = sqrt(3)

simulate <- function(hypotenuse_selection_method) {
    number_of_trials <- 10000
    trials <- 1:number_of_trials
    side_length <- sqrt(3)
    trials <- sapply(trials, function(x) hypotenuse_selection_method() >= side_length)
    return(length(trials[trials == TRUE]) / length(trials))
}

method_1 <- function() {
    p1 <- runif(1, 0, 2 * pi)
    p2 <- runif(1, 0, 2 * pi)
    angle <- min(abs(p1 - p2), 2 * pi - abs(p1 - p2))
    return(2 * sin(angle / 2))
}

method_2 <- function() {
    #angle <- runif(1, 0, 2 * pi) # useless
    point <- runif(1, 0, 1)
    return(2 * sqrt(1 - point * point))
}

method_3 <- function() {
    # https://stackoverflow.com/questions/5837572/generate-a-random-point-within-a-circle-uniformly
    r <- sqrt(runif(1, 0, 1))
    return(2 * sqrt(1 - r * r))
}

cat("probabitly calculated by simulation for method1: ", simulate(method_1), "\n")

cat("probabitly calculated by simulation for method2: ", simulate(method_2), "\n")

cat("probabitly calculated by simulation for method3: ", simulate(method_3), "\n")