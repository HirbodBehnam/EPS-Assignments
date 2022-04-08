cars <- mtcars

#### First section
cars[, "cyl"] <- sapply(seq_len(nrow(cars)), function(i) cars[i, "cyl"] * 2)

#### Second section (aggregate seems to sort the result)
print(aggregate(cars, list(cars$disp), median))

#### Third part
random_shuffle <- function(df) {
    return(df[sample(seq_len(nrow(df))), ])
}
get_top_rows_variance <- function(df, n, col) {
    return(var(df[seq_len(min(c(n, nrow(df)))), col]))
}
# There is a small problem here. Number of columns of cars is less than 100
# So in get_top_rows_variance function I added a min between n and
# number of rows of dataframe
variances_of_disp <- sapply(
    1:100,
    function(i) get_top_rows_variance(random_shuffle(cars), i, "disp")
)
print(variances_of_disp)

#### Fourth part
df <- read.csv("data.csv")
v <- subset(df, Sweet > 2.5)$Source

#### Fifth part; Pioneer is edited to correct spelling
if ("Pioneer" %in% v) {
    print("yes")
} else {
    print("no")
}