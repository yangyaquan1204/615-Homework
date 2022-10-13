myName <- "Yaquan Yang"

## ex1
library(datasets)
iris <- data.frame(iris)
iris.vers <- iris[iris$Species=="versicolor",]
ans_1 <- iris.vers
ans_1
## ex2
sepal.dif <- c(ans_1$Sepal.Length-ans_1$Sepal.Width)
ans_2 <- sepal.dif
ans_2
## ex3
iris.vers=cbind(iris.vers,sepal.dif)
ans_3 <- iris.vers
ans_3
## ex4
data <- mtcars
x <- sapply(mtcars,class)
ans_4 <- x
ans_4
## ex5
attach(mtcars)
library(dplyr)
newmtc <- mtcars %>% mutate(am = as.integer(am), cyl = as.integer(cyl), 
                            vs = as.integer(vs))
ans_5 <- sapply(newmtc,class)
ans_5
## ex6
ans_6 <- round(newmtc,1)
ans_6
## ex7
library(dplyr)
iris_7 <- filter(iris,Sepal.Width > 3.5, Species =="virginica")
ans_7 <- iris_7
ans_7
## ex8
iris_8 <- iris[iris$Species == "virginica"&iris$Sepal.Width > 3.5,1:4]
ans_8 <- iris_8
ans_8
## ex9
r_id <- row.names(iris[iris$Species == "virginica"&iris$Sepal.Width > 3.5,1:4])
ans_9 <- r_id
ans_9
## ex10
library(ggplot2)
diam_10 <- sum(diamonds$cut == "Ideal" & diamonds$carat < 0.21)
ans_10 <- diam_10
ans_10
## ex11
data = diamonds
diam_11 <- sum((diamonds$x+diamonds$y+diamonds$z) > 40)
ans_11 = diam_11
ans_11
## ex12
data = diamonds
diam_12 <- sum(diamonds$price>10000|diamonds$depth>=70)
ans_12 = diam_12
ans_12
## ex13
diam_13 <- diamonds[c(67,982), c(3,9)]
ans_13 <- diam_13
ans_13
## ex14
diam_14 <- diamonds[c(453,792,10489), ]
ans_14 <- diam_14
ans_14
## ex15
library(tibble)
diam_15 <- tibble(diamonds[c(1:10), c(8:10)])
ans_15 <- diam_15
ans_15
## ex16
data16 <- diamonds[1:1000, ]
newdiam <- as_tibble(data16)
ans_16 <- newdiam
ans_16
## ex17
data17 <- arrange(newdiam,price)
newdiam_17 <- as_tibble(data17)
ans_17 <- newdiam_17
ans_17
## ex18
library(dplyr)
set.seed(56)
diam750 = sample_n(diamonds,750)
ans_18 = diam750
ans_18

## ex19
sum_diam750 <- summary(ans_18)
ans_19 <-  sum_diam750
ans_19

