myName <- "yaquan yang"
## ex1
v1 <- c(1:20)
##
v2 <- c(20:1)
##
v3 <- seq(1,20,2)
##
a <- c(3,7,11)
v4 <- rep(a,10)
v4
##
v5 <- rep(c(3,7,11),length=31)
v5
## ex2
a <- seq(3,6,by=0.1)
x1 <- exp(a)*sin(a)
x1

## ex3
i <- c(10:100)
sum1 <- sum(i^3+4*i^2)
sum1

## ex4
label <- rep('label',times=30)
a<- c(1:30)
str1 <- paste(label,a,sep=" ")
str1

function1 <- rep('function',times=30)
b<- c(1:30)
str2 <- paste(function1,b,sep= '')
str2


## ex5
vs <- paste(c('1','function','NA', seq(1,5,2), 0.125),collapse = ',')
vs


## Matrix
## ex6
A <- matrix(1:9, ncol = 3, nrow = 3)
A
m1_ans <- A%*%A%*%A
m1_ans

## ex7
a <- c(rep(12,17))
b <- c(rep(-12,17))
c <- c(rep(12,17))
B <- matrix(c(a,b,c),nrow = 17,ncol = 3)
B_trans<- t(B)
m2_ans <- B_trans%*%B
m2_ans

## ex8
y <- c(7,-1,-3,5,17)
A<- matrix(0,nr=5, nc=5)
A <- abs(col(A)-row(A))+1
A
m3_ans <- solve(A,y)

## ex9 a
xv <- seq(0.0, 1.0, by= 0.1)

function1 <- function(xv)
{
  xv^(1:length(xv))
}

func1_ans = function1(xv)
func1_ans

## ex9 b

function2 <- function(xv)
{
  (xv^(1:length(xv))/(1:length(xv)))
}

func2_ans = function2(xv)
func2_ans

## ex9 c
function3 <- function(x,n)
{
  1+sum((x^(1:n))/(1:n))
}
func3_ans <- function3(seq(0,1,0.1),length(seq(0,1,0.1)))
func3_ans

## ex10
cel_to_far <- function(cel) {
  far<- (cel*1.8+32)
  return(far)
}
cel_to_far(30)

far_to_cel <- function(far) {
  cel<- (far-32)*5/9
  return(cel)
}
far_to_cel(50)



## ex11
func_odd <- function(x)
{
  seq(1,x,by=2)
}
odd_ans <- func_odd(2000)
odd_ans


## ex12
function12 <- function(x)
{
  v12 <- function(r){sum(((1:r)^0.5)/(11+3.5*r^1.2))}
  sum(sapply(x, FUN=v12))
}
sum_ans <- function12(10)
sum_ans

## ex13
modNumber<-function(x,y){
  while(x%%y!=0)
    x<-x+1
  return(x)
}
modNumber(500,6)

## ex14
numberOfWheels <- function(x){
  v <- switch(x, "unicycle"=1, "bike"=2, "car"=4,"truck"=4,"motorcycle"=2)
  return(v)
}
numberOfWheels("bike")

## ex15
myFactorial <- function(x){
  outcome <- factorial(x)
  return(outcome)
}
myFactorial(5)

## ex16
myCustomFactorial <- function(x,y){
  prod(y:x)
}
myCustomFactorial(5,1)

## ex17
customRiverMean <- function(max_length){
  df <- rivers
  y <- (rivers[rivers < max_length])
  output <- mean(y)
  return(output)
}

customRiverMean(300)

## ex18
df <- ToothGrowth
df
longTeeth<-c()
length18<-length(ToothGrowth$len)
for (i in 1:length18){
  if(ToothGrowth$len[i]>=15)
    longTeeth<-c(longTeeth,ToothGrowth$len[i])
}
print(longTeeth)

## ex19
df <- mtcars
a <- apply(df,2,mean)
averageHorsePower <- c(mean(mtcars$hp))
averageWeight <- c(mean(mtcars$wt))
averageHorsePower
averageWeight

## ex20
sapple = function (x,y){
  for (i in 1:length(x)){
    z[i]=length(which(y<x[i]))
  }
}