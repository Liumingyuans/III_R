# title : data mining in R
# date  : 2014.11.14,15
# author: Ming-Chang Lee
# email : alan9956@gmail.com
# RWEPA : http://rwepa.blogspot.tw/

# 1. data mining
# 2. decision tree
# 3. generalized linear model (GLM)-logistic regression
# 4. support vector machine (SVM)
# 5. dimension reduction-principal components analysis (PCA)
# 6. outlier detection
# 7. model performance analysis

# 1. data mining -----

# p.20
# Run in R, not in RStudio
# install.packages("rattle")
# library(rattle)
# rattle()

# 2. decision tree -----

# example 1. Building Decision Trees with party package -----

str(iris)
set.seed(1234)
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))
trainData <- iris[ind==1,]
testData <- iris[ind==2,]

# p.28
library(party)
# model formula
myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
# build tree
iris_ctree <- ctree(myFormula, data=trainData)
# check the prediction
table(predict(iris_ctree), trainData$Species)

# plot tree
print(iris_ctree)
plot(iris_ctree)
plot(iris_ctree, type="simple")

# predict on test data
testPred <- predict(iris_ctree, newdata = testData)
table(testPred, testData$Species)
# end

########## supplement
# 讀取節點資料
nodes(iris_ctree, 3)
nodes(iris_ctree, 4)[1]
nodes(iris_ctree, 4)[[1]]
names(nodes(iris_ctree, 4)[1])
names(nodes(iris_ctree, 4)[[1]])
nodes(iris_ctree, 5)[[1]]$prediction
nodes(iris_ctree, 6)[[1]]$prediction

########## supplement
library(party)
# import pharmacy data
# drug <- read.table(file="drug.csv", header=TRUE, sep=",")

drug <- read.table("http://web.ydu.edu.tw/~alan9956/R/drug.csv", header=TRUE, sep=",",stringsAsFactor=F)

drug$UnitPrice <- sub(",","", drug$UnitPrice) #,置換成空白字元

drug$UnitPrice <- as.numeric(drug$UnitPrice)

attach(drug)
str(drug)
dimnames(drug)[[2]]
summary(drug)
table(Inventory)

set.seed(12345)
ind <- sample(2, nrow(drug), replace=TRUE, prob=c(0.7, 0.3))
trainData <- drug[ind==1,]
testData <- drug[ind==2,]

# model formula
myFormula <- Inventory ~ UnitPrice + OrderingCost + Demand + LeadTime
# build tree
drug.ctree <- ctree(myFormula, data=trainData)
# check the prediction
table(predict(drug.ctree), trainData$Inventory)

# plot tree
print(drug.ctree)
plot(drug.ctree)
plot(drug.ctree, type="simple")

# predict on test data
testPred <- predict(drug.ctree, newdata = testData)
table(testPred, testData$Inventory)
# end

# example 2. Building Decision Trees with Random Forest -----

ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))
trainData <- iris[ind==1,]
testData <- iris[ind==2,]

# 建立隨機森林
library(randomForest)
iris.rf <- randomForest(Species ~ ., data=trainData, ntree=100, proximity=TRUE)
table(predict(iris.rf), trainData$Species)
print(iris.rf)
attributes(iris.rf)

# Error rates with various number of trees
plot(iris.rf)

# Extract variable importance measure
importance(iris.rf) # Gini index
varImpPlot(iris.rf)

# Test the built random forest on test data
irisPred <- predict(iris.rf, newdata=testData)
table(irisPred, testData$Species)
# end

# example 3 Building Decision Trees with rpart package -----

# http://archive.ics.uci.edu/ml/
# Statlog (German Credit Data)

credit <- read.csv("credit.csv")
names(credit)
str(credit)
head(credit)

levels(credit$checkingstatus1) <- c("<= 0 DM","1-200 DM", "> 200 DM", "unknown")
table(credit$checkingstatus1)

levels(credit$savings) <- c("< 100 DM","100 <= saving < 500 DM", "500 <= saving < 1000 DM", "<= 1000 DM", "no saving")
table(credit$savings)

summary(credit$duration)
summary(credit$amount)

credit$Default <- as.factor(credit$Default)
levels(credit$Default) <- c("no","yes")
table(credit$Default)

# split into training and test subsets
set.seed(1234)
ind <- sample(2, nrow(credit), replace=TRUE, prob=c(0.7, 0.3))
trainData <- credit[ind==1,]
testData <- credit[ind==2,]

# train a decision tree
library(rpart)
myFormula <- Default ~ .
credit_rpart <- rpart(myFormula, data=credit, control=rpart.control(minsplit = 10))
attributes(credit_rpart)

print(credit_rpart)
print(credit_rpart$cptable)
plot(credit_rpart)
text(credit_rpart, use.n=TRUE)

# prune the tree
opt <- which.min(credit_rpart$cptable[,"xerror"])
cp.prune <- credit_rpart$cptable[opt, "CP"]
cp.prune
credit_prune <- prune(credit_rpart, cp=cp.prune)
print(credit_prune)
plot(credit_prune)
text(credit_prune, use.n=TRUE)

# 3. 廣義線性模型 generalized linear model (GLM)-logistic regression -----

# example - Logistic regression -----

# install.packages("AER")
# library(AER)
# ?Affairs
data(Affairs, package="AER")
summary(Affairs)
table(Affairs$affairs)
head(Affairs)

# add new column
Affairs$ynaffair[Affairs$affairs > 0] <- 1
Affairs$ynaffair[Affairs$affairs == 0] <- 0
Affairs$ynaffair <- factor(Affairs$ynaffair,
                           levels=c(0,1),
                           labels=c("No","Yes"))
table(Affairs$ynaffair)

# logistic regression- consider all variables
fit.full <- glm(ynaffair ~ gender + age + yearsmarried + children + religiousness + education + occupation + rating, data=Affairs, family=binomial())

summary(fit.full)

# logistic regression- droped 4 variables
fit.reduced <- glm(ynaffair ~ age + yearsmarried 
                   + religiousness 
                   + rating, 
                   data=Affairs, family=binomial())

summary(fit.reduced)

# model comparision
anova(fit.reduced, fit.full, test="Chisq")

# interpreting the model parameters
coef(fit.reduced)

# exponentiate to put the results on an odds scale
exp(coef(fit.reduced))

# confidence interval
exp(confint(fit.reduced))

# sensitivity analysis for rating
testdata <- data.frame(rating=c(1, 2, 3, 4, 5), age=mean(Affairs$age),
                       yearsmarried=mean(Affairs$yearsmarried),
                       religiousness=mean(Affairs$religiousness))
testdata
testdata$prob <- predict(fit.reduced, newdata=testdata, type="response")
testdata

# sensitivity analysis for age
testdata <- data.frame(rating=mean(Affairs$rating),
                       age=seq(17, 57, 10),
                       yearsmarried=mean(Affairs$yearsmarried),
                       religiousness=mean(Affairs$religiousness))
testdata
testdata$prob <- predict(fit.reduced, newdata=testdata, type="response")
testdata

# try!
# dependent variable: PersonalLoan, using glm
filename <- "http://web.ydu.edu.tw/~alan9956/R/bankloan.csv"
columndetails <- read.table(filename, nrows=13, sep=",")[,c(1:2)]
x <- read.table(filename, head=TRUE, sep="," , skip=13)

# example - poisson regression - 抗癲癇藥物實驗 -----

# poisson regression
data(breslow.dat, package="robust")
names(breslow.dat)
str(breslow.dat)

# verify input data
opar <- par(no.readonly=TRUE)
par(mfrow=c(1,2))
attach(breslow.dat)
hist(sumY, breaks=20, xlab="Seizure Count",
     main="Distribution of Seizures")
boxplot(sumY ~ Trt, xlab="Treatment", main="Group Comparisons")
par(opar)

# poisson regression modeling
fit <- glm(sumY ~ Base + Age + Trt, data=breslow.dat, family=poisson())
summary(fit)

# Interpreting the model parameters
coef(fit)
exp(coef(fit))

# 4. support vector machine (SVM) -----
# refer to materials

library(e1071) #SVM
library(mlbench) #Glass data

data(Glass)
head(Glass)
index <- c(1:nrow(Glass))
testindex<-sample(index, trunc(length(index)/3))
testset <- Glass[testindex, ]      
dim(testset)[2]
trainset <- Glass[-testindex,]

#利用svm 執行並將結果存入變數Glass.svm

Glass.svm <- svm(Type ~ ., data=trainset, cost=100, gamma=1)
Glass.svm.pred <- predict(Glass.svm, testset[,-10])
testset$predclass <- Glass.svm.pred
print(Glass.svm.pred)
write.table(testset, file="Glass.testset.pred.csv", sep=",", row.name=FALSE)

# 5. principal components analysis (PCA) -----
# PCA example: European Protein Consumption
food <- read.csv("http://web.ydu.edu.tw/~alan9956/R/protein.csv")
str(food)

summary(food[-1])

pairs(food[-1])

head(food)
food[1:3]

# correlation matrix
cor(food[,-1])

# pca
pcafood <- prcomp(food[,-1], scale=TRUE)
pcafood

# read Standard deviations
names(pcafood)
pcafood$sdev
sum(pcafood$sdev^2)

# prediction
foodpc <- predict(pcafood)
foodpc

## how many principal components do we need?
plot(pcafood, main="")
mtext(side=1, "European Protein Principal Components",  line=1, font=2)

## principal components plot
par(mfrow=c(1,2))
plot(foodpc[,1:2], type="n", xlim=c(-4,5))
text(x=foodpc[,1], y=foodpc[,2], labels=food$Country)
plot(foodpc[,3:4], type="n", xlim=c(-3,3))
text(x=foodpc[,3], y=foodpc[,4], labels=food$Country)
pcafood$rotation[,2] # PC2, main for fish, starch, veg.
par(mfrow=c(1,1))

# 6. outlier detection -----

# Univariate outlier detection
?boxplot.stats
x <- read.table("http://web.ydu.edu.tw/~alan9956/R/gfc.csv", head=TRUE, sep=",")
head(x)
summary(x)

x.廣達 <- x[x$supplier=="廣達",]
boxplot.stats(x.廣達$amount)
boxplot.stats(x.廣達$amount)$out
boxplot(x.廣達$amount, horizontal=TRUE) 
summary(x.廣達$amount)

# clustering for outlier detection
set.seed(123456)
# remove species from the data to cluster
iris2 <- iris[,-5]
iris2.kmeans <- kmeans(iris2, centers=3)

# cluster centers
iris2.kmeans$centers

# cluster IDs
iris2.kmeans$cluster
iris2$cluster <- iris2.kmeans$cluster
head(iris2)

# calculate distances between objects and cluster centers
centers <- iris2.kmeans$centers[iris2.kmeans$cluster, ]
distances <- sqrt(rowSums((iris2 - centers)^2))

# pick top 5 largest distances
outliers <- order(distances, decreasing=TRUE)[1:5]

# print outliers
print(outliers)
print(iris2[outliers,])

# plot clusters
plot(iris2[,c("Sepal.Length", "Sepal.Width")], pch=1, col=iris2.kmeans$cluster, cex=0.8)

# plot cluster centers
points(iris2.kmeans$centers[,c("Sepal.Length", "Sepal.Width")], col=1:3, pch=8, cex=1.5)

# plot outliers
points(iris2[outliers, c("Sepal.Length", "Sepal.Width")], pch="+", col=4, cex=1.5)

# 7. model performance analysis -----

# R - SVM model and Back Propagation Neural Network model
library(ROCR)
data(ROCR.hiv) 
attach(ROCR.hiv) 
pred.svm <- prediction(hiv.svm$predictions, hiv.svm$labels) 
perf.svm <- performance(pred.svm, 'tpr', 'fpr') 
pred.nn <- prediction(hiv.nn$predictions, hiv.svm$labels)
perf.nn <- performance(pred.nn, 'tpr', 'fpr') 
plot(perf.svm, lty=3, col="red",main="SVMs and NNs for prediction of 
     HIV-1 coreceptor usage") 
plot(perf.nn, lty=3, col="blue",add=TRUE) 
plot(perf.svm, avg="vertical", lwd=3, col="red", 
     spread.estimate="stderror",plotCI.lwd=2,add=TRUE) 
plot(perf.nn, avg="vertical", lwd=3, col="blue", 
     spread.estimate="stderror",plotCI.lwd=2,add=TRUE) 
legend(0.6,0.6,c('SVM','NN'),col=c('red','blue'),lwd=3) 

### supplement 1. 預測墾丁國家公園遊客數 -----
library(forecast)
np <- read.table("http://web.ydu.edu.tw/~alan9956/R/taiwannp.csv", header=T, sep=",")
np <- np[1:170,]
str(np)
np.kenting <- np$墾丁
np.kenting.ts <- ts(np.kenting,start=c(2000,1),frequency=12)
plot(np.kenting.ts)
fit <- auto.arima(np.kenting.ts)
forecast(fit, 4)
forecast(fit, 4)$mean
plot(forecast(fit,4), xlab="時間", ylab="參觀人數", main="2014年3-6月預測值")
### end

### supplement 2. r and SQL database -----

# http://rwepa.blogspot.tw/2013/08/rodbc-sql-server.html

### supplement 3. sqldf package -----

# install.packages("sqldf")
library(sqldf)

# head
a1r <- head(warpbreaks)
a1s <- sqldf("select * from warpbreaks limit 6")
identical(a1r, a1s)

# subset
a2r <- subset(CO2, grepl("^Qn", Plant))
a2s <- sqldf("select * from CO2 where Plant like 'Qn%'")
all.equal(as.data.frame(a2r), a2s)

data(farms, package = "MASS")
a3r <- subset(farms, Manag %in% c("BF", "HF"))
a3s <- sqldf("select * from farms where Manag in ('BF', 'HF')")
row.names(a3r) <- NULL
identical(a3r, a3s)
a4r <- subset(warpbreaks, breaks >= 20 & breaks <= 30)
a4s <- sqldf("select * from warpbreaks where breaks between 20 and 30", row.names = TRUE)
identical(a4r, a4s)

### supplement 4. map with shp file -----

# download taiwan shp files: TWN_adm.zip
# http://www.diva-gis.org/gdata
# http://www.gadm.org/  # gadm including R sp file

# install.packages(c("sp", "maptools"))
library(sp)
library(maptools)
setwd("d:/r.data")
tw.map <- readShapeSpatial("TWN_adm2.shp")
plot(tw.map)

### supplement 5. function value -----

fns <- function(x) {
  x1 <- x[1]
  x2 <- x[2]
  fns1 <- (x1+x2)/10
  return(fns1)
}

size <- 1000000
x <- matrix(1:(size*2), byrow=TRUE, ncol=2)
str(x)
head(x)
system.time(fun.values <- apply(x, 1, fns))
head(fun.values)
# end
