# Import the dplyr library
library(dplyr)

#### First section
filtered_iris <- dplyr::filter(
    iris,
    Sepal.Length > 5, Petal.Length > 3, Species == "virginica"
)

#### Second section
Rectangularity <- abs(
    dplyr::pull(filtered_iris, "Sepal.Length") - dplyr::pull(filtered_iris, "Sepal.Width")
)

#### Third section
filtered_iris <- dplyr::mutate(
    filtered_iris,
    Sepal.Rectangularity = Rectangularity
)

#### Fourth section. There are two ways to do it
#### They are a different in the row id
print(head(filtered_iris[order(-filtered_iris$Sepal.Rectangularity), ], 7))
# OR
print(dplyr::slice(
    filtered_iris[order(-filtered_iris$Sepal.Rectangularity), ],
    1:7
))

#### Fifth section
filtered_iris <- dplyr::mutate(
    filtered_iris,
    X = mean(c(Sepal.Rectangularity, Petal.Length, Petal.Width)) < 5
)

#### Sixth section
print(data.matrix(filtered_iris))
# OR
print(as.matrix(filtered_iris))