# title  : R - basic-iii
# date   : 2014.11.1
# author : Ming-Chang Lee
# email  : alan9956@gmail.com
# RWEPA  : http://rwepa.blogspot.tw/

# R軟體基本觀念 -----

# p.8
plot(runif(100), type="l")

# demo graphics
demo(graphics)

# p.12
library(Rcmdr)

# 整合式開發環境 RStudio -----

# p.22
library(ggmap)
map.taiwan <- get_map(location="Taiwan", zoom=8)
ggmap(map.taiwan)


# p.28
# RStudio demo
weight <- seq(50, 70, length=10) + rnorm(10,5,1)
height <- seq(150, 170, length=10) + rnorm(10,6,2)
test <- data.frame(weight, height) # type [tab]
test.lm <- lm(weight ~ height, data=test)
summary(test.lm)
op <- par(mfrow=c(2,2))
plot(test.lm)
par(op)

# 物件、函數、套件、環境與輔助說明 -----

# p.32
height

# p.33
# install.packages("qcc")
example(qcc, package="qcc")

# p.34
# loaded packages
search()

# installed packages
x <- installed.packages()
x

# p.35
install.packages("e1071")
library(e1071)

# p.38
# Workspace
x <- 2013.0802
global.e <- globalenv()
global.e
ls()
ls(global.e)
global.e$x
class(x)
class(global.e)
rm(x)
ls()

# p.40
# name of object
A <- "中華R軟體學會CARS"; compar <- TRUE; z <- 3+4i
mode(A); mode(compar); mode(z)

# p.44
# A vector of five numbers
v1 <- c(.29, .30, .15, .89, .12)
v1
class(v1)
typeof(v1)
v1[1]
v1[2:4]
v1[1,3,5] # error
v1[c(1,3,5)]

# Coerces into a single type
v2 <- c(.29, .30, .15, .89, .12, "wepa")
v2
class(v2)
typeof(v2)

# vector(mode, length)
x1 <- vector(mode="numeric", length=1000000)
# View x1
head(x1)
# Verify a vector
is.vector(x1)

# vector
x2 <- c("Taiwan", "China", "USA")
x2
is.vector(x2)

# Expand the length of a vector
length(x2) <- 5
x2

# factor
f1 <- factor(1:3)
f2 <- factor(1:3, levels=1:5)
f3 <- factor(1:3, labels=c("A", "B", "C"))
f4 <- factor(letters[1:6], label="YDU")
f4
class(f4)
eye.colors <- factor(c("brown", "blue", "blue", "green", 
                       "brown", "brown", "brown"))
eye.colors
levels(eye.colors)

# matrix
matrix.data <- matrix(c(1,2,3,4,5,6), 
                      nrow = 2, ncol = 3, byrow=TRUE, 
                      dimnames = list(c("row1", "row2"), c("C1", "C2", "C3")))
matrix.data

# array
a1 <- array(letters)
a1
class(a1)
dim(a1) #26

a2 <- array(1:3, c(2,4)) # 2 rows, 4 columns
a2
dim(a2) # 2, 4
length(a2)
a2[1, ] # select row 1
a2[, 4] # select column 4

a3 <- array(data=1:24,dim=c(3,4,2))
a3
class(a3)
str(a3)

# data.frame
x <- c(1:4); n <- 10; m <- c(10, 35); y <- c(2:4)
df1 <- data.frame(x, n)
df1
df2 <-data.frame(x, m)
df2

# The data give the speed of cars and 
# the distances taken to stop.
data(cars)
cars
# help(cars)
class(cars)
head(cars)
head(cars, n=3)

# list
list.test1 <- list(1,2,3,4,5)
list.test1
str(list.test1)
list.test1[5]   # []: key + values
list.test1[[5]] # [[]]: values

# Each element in a list may be given a name
product <- list(destination="Taipei",
                dimensions=c(3,6,10),price=712.19)
product[2]
product[[2]]
product$price

# p.49
# Special numbers: Inf, -Inf, NaN, NA
x <- 5/0
x
exp(x)
exp(-x)
x - x
0/0

# NA
x.NA <- c(1,2,3)
x.NA
length(x.NA) <- 4
x.NA

month.abb
month.name

paste(c(1:12), "月", sep="")

# p.52
# Time-series
data.ts <- ts(c(2,5,4,6,3,7,9,8),start=c(2009,2),frequency=4)
data.ts
is.ts(data.ts)
start(data.ts)
end(data.ts)
frequency(data.ts)
deltat(data.ts) # 0.25(=1/4)
plot(data.ts, type="b")

# build an ARIMA model
fit <- arima(AirPassengers, order=c(1,0,0), list(order=c(2,1,0), period=12))
fore <- predict(fit, n.ahead=24)
# error bounds at 95% confidence level
U <- fore$pred + 2*fore$se
L <- fore$pred - 2*fore$se
ts.plot(AirPassengers, fore$pred, U, L, col=c(1,2,4,4), lty = c(1,1,2,2), ylab="Passengers(1000)")
legend("topleft", col=c(1,2,4), lty=c(1,1,2),
       c("Actual", "Forecast",
         "Error Bounds (95% Confidence)"))

# p.53
# vectoried operation
x <- c(1:10)
(x.square <- x^2)

# p.54
c(1,2,3,4) + c(4,5)
x <- matrix(c(1:6), ncol=2)
x
x + c(5,6)

# which, any, all
# p.55
(x <- matrix(rnorm(10)*10, ncol=2))
x>5
# !(x>5)
which(x>5) # return index
which(x>5, arr.ind=TRUE) # return (row,column)
any(x>5)
all(x>5)

# p.56
# apply
m <- matrix(c(1:8), ncol=2)
m
apply(m, 1, function(x) mean(x))
as.matrix(apply(m, 1, function(x) mean(x)))

# p.60-62
# function example 1, 2, 3
# refer to materials

# p.64
# S3 - object-oriented programming
set.seed(168)
layout(matrix(c(1,1,2:5,6,6),4,2, byrow=TRUE))
weight <- seq(50, 70, length=10) + rnorm(10,5,1)
height <- seq(150, 170, length=10) + rnorm(10,6,2)
test <- data.frame(weight, height) # type [tab]
test.lm <- lm(weight ~ height, data=test)
class(test.lm)
class(AirPassengers)
plot(height, weight)
plot(test.lm)
plot(AirPassengers)
layout(c(1))

# p.65
# create a matrix
a <- matrix(c(2,3,-2,1,2,2), 3, 2)
a
# verify a matrix
is.matrix(a)
is.vector(a)

# multiplication by a scalar
c <- 3
c*a

# matrix addition and substracion
(b <- matrix(c(2,1,-2,1,2,1),3, 2))
a
m.add <- a+b
m.add
m.sub <- a-b
m.sub

# matrix multiplication
(d <- matrix(c(2,-2,1,2,3,1),2,3))
(m.product <- d %*% a)

# transpose matrix
a.trans <- t(a)
a.trans

# unit vector
m.unit <- matrix(1,3,1)
m.unit

# unit matrix
m.comm <- matrix(1,3,2)
m.comm

# diagonal
s <- matrix(c(2,3,-2,1,2,2,4,2,3),3,3)
s
m.diag <- diag(s)
m.diag
class(m.diag) # numeric
as.matrix(m.diag)

# 資料匯入、儲存/載入、匯出 -----

# p.69
# Import/Export data
# Create a data directory C:\R.data
# Get working directory
getwd()
# Set working directory
workpath <- "C:/R.data"
setwd(workpath)
getwd()

# p.72
# Create a CSV file (C:\R.data\score.csv)
# in which each field is separated by commas.
# Import dataset
score1 <- read.table(file="score.csv", header= TRUE, sep=",")
# TRY !
# score1 <- read.table(file="score.csv", header= TRUE)
score1
dim(score1)
names(score1)
row.names(score1)

# p.73
# Add new column data for mid_term
mid.term <- matrix(c(60,80,65,85,80,90,99), nrow=7, ncol=1, byrow=FALSE,   dimnames = list(c(),c("mid.term")))
mid.term
# Merge two data.frame( score1 and mid_term)
score2 <- data.frame(score1, mid.term)
score2

# p.74
# Export dataset
write.table(score2 , file= "score.final.txt", 
            sep = "\t", 
            append=FALSE, 
            row.names=FALSE, 
            col.names = TRUE, 
            quote= FALSE)

# p.77
# install.packages("XLConnect") # 17.1MB
library(XLConnect)
vignette ("XLConnect")

# loadWorkbook(filename, create=TRUE)
# createSheet(object, name)
# writeWorksheet(object, data, sheet, startRow=1, startCol=1, header=TRUE)
# saveWorkbook(object)
wb <- loadWorkbook("XLConnectExample1.xlsx", create = TRUE)
createSheet(wb, name="chickSheet")
writeWorksheet(wb, ChickWeight, sheet="chickSheet", startRow=3, startCol=4)
saveWorkbook(wb)

excelname <- file.choose()
sheetname <- "sheetname"
excel.data <- loadWorkbook(excelname, create=TRUE)
x <- readWorksheet(excel.data, sheet=sheetname)

# load spss file
library(foreign)
db <- read.spss("http://web.ydu.edu.tw/~alan9956/R/school.sav", to.data.frame=TRUE)
head(db)
str(db)

# 機率分配與抽樣 -----

rnorm(3) # 建立3個標準常態分配隨機樣本
pnorm(0.7, 1, 1) #常態分配 0.7左尾累積機率值
pnorm(0.7, lower.tail=F) # 右尾累積機率值
qnorm(0.95) # 1.645

# p.92
plot(dnorm, -3, 3, main = "常態機率分配")

# p.93
hist(rnorm(10000), breaks=50)

# 輪胎耐磨性測試資料 -----
# p.97
library(nutshell)
data(tires.sus)
str(tires.sus)

View(tires.sus)

?tires.sus

# Time_To_Failure: the time before each tire failed (in hours)
# Speed_At_Failure_km_h: the testing speed at which the tire failed
# t test

# p.102
times.to.failure.h <- subset(tires.sus, Tire_Type=="H" & Speed_At_Failure_km_h==160)$Time_To_Failure

times.to.failure.h
mean(times.to.failure.h)

t.test(times.to.failure.h, mu=9)

# 獨立樣本 t 檢定-方法1 -----

times.to.failure.e <- subset(tires.sus, Tire_Type=="E" & Speed_At_Failure_km_h==180)$Time_To_Failure

times.to.failure.d <- subset(tires.sus, Tire_Type=="D" & Speed_At_Failure_km_h==180)$Time_To_Failure

# 比較 types D and E
t.test(times.to.failure.e, times.to.failure.d)

# Try: 比較 types E and B
times.to.failure.b <- subset(tires.sus, Tire_Type=="B" & Speed_At_Failure_km_h==180)$Time_To_Failure

t.test(times.to.failure.e, times.to.failure.b)

# 獨立樣本 t 檢定-方法2 -----

data(field.goals)
good <- transform(
  field.goals[field.goals$play.type=="FG good", c("yards","stadium.type")],
  outside=(stadium.type=="Out"))

bad <- transform(
  field.goals[field.goals$play.type=="FG no", c("yards","stadium.type")],
  outside=(stadium.type=="Out"))

head(good)
head(bad)

t.test(yards~outside,data=good) # 不顯著

t.test(yards~outside,data=bad) # 不顯著


# 成對樣本 t 檢定 -----

data(SPECint2006) # 1233*9
# str(SPECint2006)
# head(SPECint2006)
t.test(subset(SPECint2006,Num.Chips==1&Num.Cores==2)$Baseline,
       subset(SPECint2006,Num.Chips==1&Num.Cores==2)$Result,
       paired=TRUE)


# 兩樣本變異數檢定 -----

field.goals.inout <- transform(field.goals,
                               outside=(stadium.type=="Out"))
var.test(yards~outside, data=field.goals.inout)


# 逐對 t 檢定 (k-Sample Test+Pairwise) -----

pairwise.t.test(tires.sus$Time_To_Failure,tires.sus$Tire_Type)


# 常態性檢定(Shapiro-Wilk Normality Test) -----

par(mfrow=c(1,2))
hist(field.goals$yards,breaks=25)
qqnorm(field.goals$yards,pch=".")

results <- shapiro.test(field.goals$yards)
names(results)
results$p.value # p-value <= 0.05 表示 reject H0, and x deviates normal.
# end
