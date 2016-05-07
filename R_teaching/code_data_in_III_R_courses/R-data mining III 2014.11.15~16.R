### Prof. Ching-Shih Tsou (Ph.D. of IE/OR from National Taiwan Institute of Technology since 1994) at the IDS, NTUB(國立臺北商業大學資訊與決策科學研究所); the CARS(中華R軟體學會) and the DSBA(台灣資料科學與商業應用協會)

### Supplement 1: Accessing various data structures (copied from CPU .R)
# vector indexing
x <- 1:5
x
x[c(1,2,2,3,3,3,4,4,4,4)]
names(x) <- c("one", "two", "three", "four", "five")
x[4]
x[-4]
x[1:4]
x[-(1:4)]
x[c(1,4,2)]
x["four"]
x[x>3]
x[x > 3 & x < 5]
x[x %in% c(1, 3, 5)]

# list indexing
x <- list(one=1, two=2, three=3, four=4, five=5)
x
x[4]
x[[4]]
x["four"] # same as x[4]
x$four  #same as x[[4]]

# matrix indexing
x <- matrix(1:12, nrow=3, ncol=4)
x
dimnames(x) <- list(paste("row", 1:3, sep=''),paste("col", 1:4, sep=''))
x[3,4]
x[3,]
x[,4]
x[,c(1,3)]
x["row3",]
x[,"col4"]

# logical indexing
x <- 1:10
x[c(T,T,T,F,F,F,F,F,F,F)] # T: keep it; F: drop out
x[c(rep(T,3), rep(F,7))]
x <= 3
x[x <= 3]

### Supplement 2: Finding help (should be included in Basics course)
help.start() # Manuals, Reference, and Miscellaneous Material

help('plot') # function name already known (or help(plot))
?plot # an alias of help

help.search('plot') # function name unknown, search string from Vignette Names, Code Demonstrations, and Help Pages (titles and keywords)
??plot # an alias of help.search

apropos('plot') # function name with 'plot'

find('plot') # It is a different user interface to the same task. simple.words: logical; if TRUE, the what argument is only searched as whole word.
find('plot', simple.words = FALSE) # look for packages with function 'plot'

### Supplement 3: Concept of masking (should be included in Basics course)
library(psych)
describe # psych::describe
describe(iris)
Hmisc::describe(iris)

### Supplement 4: Detaching and removing package (should be included in Basics course)
search()
?detach
detach(package:psych)

remove.packages("psych")
library(psych)

install.packages('psych')

### Supplement 5: S3 - object-oriented programming (should be included in Basics course)
set.seed(168)
layout(matrix(c(1,1,2:5,6,6),4,2, byrow=TRUE))
weight <- seq(50, 70, length=10) + rnorm(10,5,1)
height <- seq(150, 170, length=10) + rnorm(10,6,2)
test <- data.frame(weight, height) # type [tab]
test.lm <- lm(weight ~ height, data=test)
class(test.lm)
class(AirPassengers)
plot(test)
plot(test.lm)
plot(AirPassengers)
layout(c(1))

### 1. Clustering and market segmentation
# 動畫示例
library(animation)
kmeans.ani()

# well-separated clusters
library(cba)  # for dataset Votes
library(help=cba) # Clustering for Business Analytics
data(Votes)
str(Votes)
head(Votes)
library(plyr) # for function ddply
ddply(Votes, 'Class', function(x)summary(na.omit(x)))



teens <- read.csv('c://R/r.data/snsdata.csv') # please choose snsdata.csv
str(teens)
summary(teens) # NAs only in gender and age
# look at missing data for gender variable
sum(is.na(teens$gender)) # 2724 NAs
table(teens$gender)
table(teens$gender, useNA = "ifany") # 9% of NAs, female is five times of male
# look at missing data for age variable
summary(teens$age) # some illogical ages
teens$age <- ifelse(teens$age >= 13 & teens$age < 20, teens$age, NA) # 5523 - 5086 = 437 NAs added
summary(teens$age)
# reassign missing gender values to "unknown"
teens$female <- ifelse(teens$gender == "F" & !is.na(teens$gender), 1, 0)
teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)
# check our recoding work
table(teens$gender, useNA = "ifany")
table(teens$female, useNA = "ifany")
table(teens$no_gender, useNA = "ifany")
mean(teens$age) # doesn't work, return 'NA'
mean(teens$age, na.rm = TRUE)
aggregate(data = teens, age ~ gradyear, FUN = mean, na.rm = TRUE) # finding the mean age by cohort
# calculating the expected age for each person according to her/his gradyear
ave_age <- ave(teens$age, teens$gradyear, FUN = function(x) mean(x, na.rm = TRUE)) # you have to create a function, otherwise...
teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)
# check the summary results to ensure missing values are eliminated
summary(teens$age) # reasonable statistics
interests <- teens[5:40] # gradyear, gender, age, friends, female, no_gender are omitted
head(interests)
interests_z <- as.data.frame(lapply(interests, scale))
head(interests_z)
# set.seed(1234)
teen_clusters <- kmeans(interests_z, centers=5, nstart = 10) # because of the film "The Breakfast Club"
# Can parallelize above clustering procedure?
str(teen_clusters)
names(teen_clusters)
teen_clusters$cluster[1:300]
teen_clusters$size # look at the size of the clusters
#集群分析結果需要人的介入解讀
teen_clusters$centers # look at the cluster centers
# append the cluster IDs to the original data frame
teens$cluster <- teen_clusters$cluster
# look at the first five records
teens[1:5, c("cluster", "gender", "age", "friends")]
aggregate(data = teens, age ~ cluster, FUN = 'mean', na.rm = TRUE) # mean age by cluster, not vary much by cluster
aggregate(data = teens, female ~ cluster, FUN = 'mean') # proportion of females by cluster
aggregate(data = teens, friends ~ cluster, FUN = 'mean') # mean number of friends by cluster

### Can make a table for the five highest words in each cluster?

apply(teen_clusters$centers, 1, order, decreasing=T)
word_idx <- apply(teen_clusters$centers, 1, order, decreasing=T)[1:10,]
word_name <- names(teens)[-(c(1:4,41:43))]
word_name[word_idx[,1]]
word_name[word_idx[,2]]
word_name[word_idx[,3]]
word_name[word_idx[,4]]
word_name[word_idx[,5]]

### 2. Association rules mining and market basket analysis
lastfm <- read.csv("c://R/r.data/lastfm.csv")
# lastfm <- read.csv(file.choose()) # choose lastfm.csv
str(lastfm) # 289955 obs. of  4 variables
lastfm[15:20,]
length(unique(lastfm$user)) # 15000 users
lastfm$user <- factor(lastfm$user) # from integer to factor
nlevels(lastfm$user) # 15000 levels (users)
nlevels(lastfm$artist) # 1004 artists (or bands)

library(arules)
playlist <- split(x=lastfm[,"artist"],f=lastfm$user) ## split a vector into groups, wow ! It's a large list of 938.3 Mb
str(playlist) # A large list, the artists each user bought !
playlist[1:2] # the first two listeners (1 and 3) listen to the following bands
duplicatedArtist <- function(x) {length(x)!=length(unique(x))} # a function for finding duplicatedArtist
sum(sapply(playlist, duplicatedArtist)) # 2
which(sapply(playlist, duplicatedArtist)) # 6980 (5290) & 9753 (7422)
playlist[[7422]]
unique(playlist[[7422]])
duplicated(playlist[[7422]]) # 17th artist (james brown) is same as the 8th artist (james brown)
playlist <- lapply(playlist,unique) ## remove artist duplicates
# if you skip this line, you will get "Error in asMethod(object): can not coerce list with transactions with duplicated items"
class(playlist) # class of "list

playlist <- as(playlist,"transactions")
class(playlist) # class of "transaction" defined in package "arules" (S4 Object-Oriented programming)

inspect(playlist[1])
## view this as a list of "transactions"
## transactions is a data class defined in arules
# large transactions (15000 elements, 2.9 Mb)
itemFrequency(playlist)
class(itemFrequency(playlist)) # a **named** numeric vector
sort(itemFrequency(playlist), decreasing=T)[1:10]

itemFrequencyPlot(playlist,support=.08,cex.names=1.5)
itemFrequencyPlot(playlist,support=.08,cex.names=1.5, topN=12) # which one is better?


musicrules <- apriori(playlist,parameter=list(support=.01,confidence=.5)) # only rules with support > 0.01 and confidence > .50

musicrules

inspect(musicrules) # 50 rules

quality(musicrules)

# association rules filtering
inspect(subset(musicrules, subset=lift > 5)) # subset(原集合, subset = 子集條件)
inspect(sort(subset(musicrules, subset=lift > 5), by="confidence"))
### Can you filter out redundnat rules and visulaize the final results?

### 3. Recommendation systems and collaborative filtering
library(recommenderlab) # package being studied
library(ggplot2) # for function 'qplot'
data(MovieLense) # Load the data we are going to work with
MovieLense # class "realRatingMatrix"
# The 100k MovieLense ratings data set. The data was collected through the MovieLens web site (movielens.umn.edu) during the seven-month period from September 19th, 1997 through April 22nd, 1998. The data set contains about 100,000 ratings (1-5) from 943 users on 1664 movies.
showMethods(class="realRatingMatrix")
as(MovieLense[1,], "list")[[1]][1:10] # please compare with as(MovieLense[1,], "list")
image(sample(MovieLense[,1501:1550], 50), main = "Raw ratings") # you can sample and visulize it several times
# Visualizing ratings
qplot(getRatings(MovieLense), binwidth = 1, main = "Histogram of ratings", xlab = "Rating")
summary(getRatings(MovieLense)) # Skewed to the left because mean is smaller than median
qplot(getRatings(normalize(MovieLense, method = "Z-score")), main = "Histogram of normalized ratings", xlab = "Rating")
summary(getRatings(normalize(MovieLense, method = "Z-score"))) # seems better

# How many movies did people rate on average
qplot(rowCounts(MovieLense), binwidth = 10, main = "Movies Rated on average", xlab = "# of movies rated", ylab = "# of users")
# Seems people get tired of rating movies at a logarithmic pace. But most rate some.
summary(rowCounts(MovieLense)) # try binwidth = 50
class(rowCounts(MovieLense))
length(rowCounts(MovieLense)) # 943
rowCounts(MovieLense)[1:10]

# What is the mean rating of each movie
qplot(colMeans(MovieLense), binwidth = .1, main = "Mean rating of Movies", xlab = "Rating", ylab = "# of movies")
# The big spike on 1 suggests that this could also be intepreted as binary
# In other words, some people don't want to see certain movies at all.
# Same on 5 and on 3.
# We will give it the binary treatment later
summary(colMeans(MovieLense))
class(colMeans(MovieLense))
length(colMeans(MovieLense)) # 1664
colMeans(MovieLense)[1:10]

recommenderRegistry$get_entry_names()

recommenderRegistry$get_entries(dataType = "realRatingMatrix")
# We have a few options
# Let's check some algorithms against each other
scheme <- evaluationScheme(MovieLense, method = "split", train = .9, k = 1, given = 10, goodRating = 4) # k: number of folds/times to run the evaluation (defaults to 10 for cross-validation and bootstrap and 1 for split); given: single number of items given for evaluation or a vector of length of data giving the number of items given for each observation.
scheme
algorithms <- list("random items" = list(name="RANDOM", param=list(normalize = "Z-score")), "popular items" = list(name="POPULAR", param=list(normalize = "Z-score")), "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score", method="Cosine", nn=50, minRating=3)), "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score"))) # please take a look at ?evaluate
# run algorithms, predict next n movies
results <- evaluate(scheme, algorithms, n=c(1, 3, 5, 10, 15, 20)) # IBCF is the most time-consuming one

results <- evaluate(scheme, algorithms, n=c(10, 30, 50, 100, 150, 200)) # IBCF is the most time-consuming one
class(results) # class "evaluationResultList"
names(results)
show(results) # same as results
results[['popular items']] # S4 OO for better security !

class(results[['popular items']])
show(results[['popular items']]) # same as results[['popular items']]

getConfusionMatrix(results[['random items']])
getRuns(results[['random items']])

getConfusionMatrix(results[['popular items']])
getRuns(results[['popular items']])

getConfusionMatrix(results[['user-based CF']])
getRuns(results[['user-based CF']])

getConfusionMatrix(results[['item-based CF']])
getRuns(results[['item-based CF']])

# Draw ROC curve
plot(results, annotate = 1:4, legend="topleft")
# See precision / recall (y = "prec/rec")
plot(results, "prec/rec", annotate=3) # what does the argument "annotate" do?
# It seems like UBCF did better than IBCF. Then why would you use IBCF?

### 4. Outliers detection by density-based Clustering
library(fpc) # 'fpc': Flexible Procedure for Clustering, for function 'dbscan', Density-Based Spatial Clustering And Noises
str(iris) # a built-in data set in package {datasets}
head(iris) # data understanding
iris2 <- iris[-5]
ds <- dbscan(iris2, eps=0.42, MinPts=5) # three clusters
ds <- dbscan(iris2, eps=0.22, MinPts=5) # two clusters with many noises
ds <- dbscan(iris2, eps=0.84, MinPts=5) # two clusters with only one noise
str(ds) # "dbscan", a Density-Based Spatial Clustering of Applications with Noise
ds
table(ds$cluster, iris$Species)
plot(ds, iris2) # a generic function, ?plot.dbscan
?plot.dbscan # plot.dbscan distinguishes between seed and border points by plot symbols
plot(ds, iris2[c(1,4)]) # red: C1 (border+seed), green: C2 (border+seed), blue: C3 (border+seed), black circle: border
iris2$cluster <- ds$cluster
rownames(iris2)[iris2$cluster==0]

## label new data
set.seed(435)
idx <- sample(1:nrow(iris), 10) # randomly select ten obs.
newData <- iris[idx, -5]
newData
newData <- newData + matrix(runif(10*4, min=0, max=0.2), nrow=10, ncol=4) # add small noise to them
newData
myPred <- predict(ds, iris2, newData) # label new data, a vector of predicted cluster numbers for the point in newdata
myPred
plot(iris2[c(1,4)], col=1+ds$cluster) # ds$cluster: 0 ~ 3
points(newData[c(1,4)], pch="*", col=1+myPred, cex=3) # myPred: 0 ~ 3
table(myPred, iris$Species[idx]) # ? assigned with correct class labels, ? with incorrect class label, and ? classified as a noise

### 5. Classification trees
library(tree)
iristree <- tree(Species ~ ., data = iris)
iristree
plot(iristree)
text(iristree)

irissnip <- snip.tree(iristree, nodes=c(7,12)) # Petal.Width > 1.75 (virginica, virginica) & Petal.Length < 4.95 (versicolor, versicolor)
plot(irissnip, col='blue')
text(irissnip, digits=2)

library(rpart) # 'rpart' stands for recursive partitioning
iristree <- rpart(Species ~ ., data = iris)
library(rpart.plot)
library('rattle') # Fancy tree plot
fancyRpartPlot(iristree)
?formula # model formulae

### 5. Credit risk management by C5.0
credit <- read.csv(file.choose()) # select credit.csv
str(credit)
# look at two characteristics of the applicant
prop.table(table(credit$checking_balance, credit$default))
prop.table(table(credit$savings_balance, credit$default))
# It seems like a safe assumption that larger checking and savings account balances should be related to a reduced chance of loan default
# look at two characteristics of the loan
summary(credit$months_loan_duration)
summary(credit$amount)
# look at the class variable
table(credit$default)
# create a random sample for training and test data
# use set.seed to use the same random number sequence as the tutorial
set.seed(12345)
credit_rand <- credit[order(runif(1000)), ]
summary(credit$amount) # compare the credit and credit_rand data frames
summary(credit_rand$amount)
head(credit$amount) # compare the credit and credit_rand data frames
head(credit_rand$amount)
credit_train <- credit_rand[1:900, ] # split the data frames
credit_test  <- credit_rand[901:1000, ]
# check the proportion of class variable
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))
library(C50)
credit_model <- C5.0(credit_train[-17], credit_train$default) # build the default decision tree
credit_model # display simple facts about the tree

# display detailed information about the tree

credit_model <- C5.0(credit_train[-17], credit_train$default, control=C5.0Control(winnow=T,minCases=10)) # build a simpler decision tree

credit_model

summary(credit_model)

# create a factor vector of predictions on test data
credit_pred <- predict(credit_model, credit_test)
credit_pred

table(credit_pred, credit_test$default)

library(gmodels) # Various R programming tools for model fitting
CrossTable(credit_test$default, credit_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default')) # Cross Tabulation with Tests for Factor Independence
credit_boost10 <- C5.0(credit_train[-17], credit_train$default, trials = 10) # boosted decision tree with 10 trials
credit_boost10
credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default')) # error rate is 23%
credit_boost100 <- C5.0(credit_train[-17], credit_train$default, trials = 100) # boosted decision tree with 100 trials
credit_boost100
credit_boost_pred100 <- predict(credit_boost100, credit_test)
CrossTable(credit_test$default, credit_boost_pred100, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default')) # error rate is 22%
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2)
error_cost # a cost matrix
credit_cost <- C5.0(credit_train[-17], credit_train$default, costs = error_cost) # apply the cost matrix to the tree
credit_cost
summary(credit_cost)
credit_cost_pred <- predict(credit_cost, credit_test)
CrossTable(credit_test$default, credit_cost_pred, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))

### 5. Wine quality rating by regression trees and model trees
wine <- read.csv(file.choose()) # select whitewines.csv
str(wine)
hist(wine$quality)
summary(wine)

hist(wine$volatile.acidity, prob=TRUE, ylim = c(0,6))
lines(density(wine$volatile.acidity))

op <- par(mfrow=c(1,2))
hist(wine$volatile.acidity, prob=TRUE, ylim = c(0,6))
plot(density(wine$volatile.acidity))
par(op)

wine_train <- wine[1:3750, ]
wine_test <- wine[3751:4898, ]
library(rpart)
(m.rpart <- rpart(quality ~ ., data = wine_train))
summary(m.rpart)
library(rpart.plot)
rpart.plot(m.rpart, digits = 3)
rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)
p.rpart <- predict(m.rpart, wine_test)
p.rpart[1:10]
summary(p.rpart)
summary(wine_test$quality)
cor(p.rpart, wine_test$quality)

MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))
} # mean absolute error

MAE(p.rpart, wine_test$quality)

mean(wine_train$quality) # result = 5.87
MAE(5.87, wine_test$quality)
# train a M5' Model Tree

library(RWeka)
m.m5p <- M5P(quality ~ ., data = wine_train)
m.m5p
summary(m.m5p)

WOW('M5P')
?WOW

p.m5p <- predict(m.m5p, wine_test)
summary(p.m5p)

cor(p.m5p, wine_test$quality)
MAE(wine_test$quality, p.m5p)

### Text mining (moved to Naive Bayes)
### 6. Dimensionality Reduction
### Principal components analysis (PCA) -----
# PCA example: European Protein Consumption
food <- read.csv("/R/r.data/protein.csv")
str(food)

summary(food[-1])
head(food)
food[1:3]

# correlation matrix
cor(food[,-1])

#correlation matrix 視覺化method 1
#library(corrgram)
#corrgram(food[,-1], order=T)  

#correlation matrix 視覺化method 2
#library(corrplot)
#corrplot(cor(food[-1]))
#corrplot(cor(food[-1]),tl.col="black", tl.srt=45)

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

### PCA another example
ir.pca <- princomp(log(iris[, -5]),cor = T)
summary(ir.pca)
loadings(ir.pca)
ir.pc <- predict(ir.pca)
plot(ir.pc[, 1:2], xlab = "first principal component",ylab = "second principal component")
text(ir.pc[, 1:2], labels = as.character(iris[,5]), col = as.numeric(iris[,5]))

### Kernel PCA
library(kernlab)
test <- sample(1:150, 20)
kpc <- kpca(~., data = iris[-test,-5], kernel = "rbfdot", kpar = list(sigma = 0.2), features = 2)
plot(rotated(kpc), col = as.integer(iris[-test,5]), xlab = "1st Principal Component", ylab = "2nd Principal Component")
text(rotated(kpc), labels = as.character(iris[-test,5]), col = as.numeric(iris[-test,5]))
library(kernlab)
data(reuters)
kpc <- kpca(reuters, kernel = "stringdot", kpar = list(length = 5), features = 2)
plot(rotated(kpc), col = as.integer(rlabels), xlab = "1st Principal Component", ylab = "2nd Principal Component") # acq and crude almost well separated !
library(kernlab)
data(spirals)
plot(spirals)
sc <- specc(spirals, centers = 2)
sc
plot(spirals, col = sc)
sc <- specc(reuters, kernel = "stringdot", kpar = list(length = 5), centers = 2)
library(e1071) # for matchClasses
matchClasses(table(sc, rlabels))
par(mfrow = c(1, 2))
kpc <- kpca(reuters, kernel = "stringdot", kpar = list(length = 5), features = 2)
plot(rotated(kpc), col = as.integer(rlabels), xlab = "1st Principal Component", ylab = "2nd Principal Component")
plot(rotated(kpc), col = as.integer(sc), xlab = "1st Principal Component", ylab = "2nd Principal Component")

### MDS
X <- matrix(c(3,5,6,1,4,2,0,0,7,2,4,1,2,1,7,2,4,6,6,1,4,1,0,1,3,5,1,
              4,5,4,6,7,2,0,6,1,1,3,1,3,1,3,6,3,2,0,1,5,4,1), nrow = 10, ncol = 5)
(D <- dist(X))
cmdscale(D, k = 9, eig = TRUE) #classical md scale
max(abs(dist(X) - dist(cmdscale(D, k = 5))))
X_m <- cmdscale(dist(X, method = "manhattan"), k = nrow(X) - 1, eig = TRUE)
(X_eigen <- X_m$eig)
cumsum(abs(X_eigen))/sum(abs(X_eigen))
cumsum(X_eigen^2)/sum(X_eigen^2)

library(HSAUR2)
airdist <- source("c://R/r.data/chap5airdist.dat")$value
airline_mds <- cmdscale(airdist, k = 9, eig = TRUE)
airline_mds$points
(lam <- airline_mds$eig)
cumsum(abs(lam))/sum(abs(lam))
cumsum(lam^2)/sum(lam^2)

### SVD
ratings <- matrix(c(rep(c(0,0,0,1,2,5,1),3),rep(c(2,3,1,0,0,0,0),2)), 7, 5)
dimnames(ratings) <- list(c('Ed', 'Peter', 'Tracy', 'Fan', 'Ming', 'Pachi', 'Jocelyn'), c('Unagi Don', 'Chicken Katsu', 'Chirashi', 'Tri Tip', 'Pulled Pork'))
ratings
svd(ratings)
u <- svd(ratings)$u
v <- svd(ratings)$v
d <- diag(svd(ratings)$d) # 可用diag函數產生對角矩陣
d
(u%*%d%*%t(v)) # 最後一個矩陣v需要轉置7*5, 5*5, 5*5

svd_ex <- matrix(c(4,4,3,4,4,3,4,2,5,4,5,3,5,4,5,4,4,5,5,5,2,4,4,4,3,4,5),9,3)
dimnames(svd_ex) <- list(paste("hole", 1:9),c("Phil","Tiger","Vijay"))
svd_ex
svd(svd_ex)
u <- svd(svd_ex)$u
v <- svd(svd_ex)$v
d <- diag(svd(svd_ex)$d) # 可用diag函數產生對角矩陣
d
(u%*%d%*%t(v)) # 最後一個矩陣v需要轉置9*3, 3*3, 3*3

##### A Bioinformatics Case: the NCI60 data
library(ISLR)
help(NCI60)
str(NCI60)
nci.labs <- NCI60$labs # 64 cancer types
nci.data <- NCI60$data # 64 cancer cell lines * 6830 genes
dim(nci.data)
nci.labs[1:4]
table(nci.labs)

# PCA on the NCI60 Data

pr.out <- prcomp(nci.data, scale=TRUE)
names(pr.out)

Cols <- function(vec){
  cols=rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}
par(mfrow=c(1,2))
plot(pr.out$x[,1:2], col=Cols(nci.labs), pch=19, xlab="Z1", ylab="Z2") # Observations belonging to a single cancer type tend to lie near each other in this low-dimensional space
plot(pr.out$x[,c(1,3)], col=Cols(nci.labs), pch=19, xlab="Z1", ylab="Z3")
par(mfrow=c(1,1))

summary(pr.out)

plot(pr.out) # plot the variance explained by the first few principal components
pve <- 100*pr.out$sdev^2/sum(pr.out$sdev^2) # proportion of variance explained

par(mfrow=c(1,2))
plot(pve,  type="o", ylab="PVE", xlab="Principal Component", col="blue") # a scree plot
plot(cumsum(pve), type="o", ylab="Cumulative PVE", xlab="Principal Component", col="brown3") # there may be little benifit to examining more than seven or so PCs
par(mfrow=c(1,1))
## HCA & k-means continued in CPU .R

### 7. Naive Bayesian Classifier
sms_raw <- read.csv(file.choose()) # select sms_spam.csv
str(sms_raw)
# sms_raw$type <- factor(sms_raw$type)
str(sms_raw$type)
table(sms_raw$type)
prop.table(table(sms_raw$type)) # about 13% labeled as spam
library(tm) # text mining
sms_corpus <- Corpus(VectorSource(sms_raw$text))
print(sms_corpus) # same as sms_corpus
inspect(sms_corpus[1:6])
corpus_clean <- tm_map(sms_corpus, tolower)
inspect(corpus_clean[1:6])
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords()) # The default is the English stopwords
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)
inspect(sms_corpus[1:6])
inspect(corpus_clean[1:6])
sms_dtm <- DocumentTermMatrix(corpus_clean)
sms_dtm # Sparsity is 0.9990265
idx <- which(dimnames(sms_dtm)$Term == 'free')
as(sms_dtm[5:10, idx:(idx + 5)], "matrix")
sms_raw_train <- sms_raw[1:4169, ]
sms_raw_test  <- sms_raw[4170:5559, ]
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test  <- sms_dtm[4170:5559, ]
sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test  <- corpus_clean[4170:5559]
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))

#文字雲視覺化
library(wordcloud)
rainbowLevels <- rainbow(length(sms_corpus_train)) # the number of colors (≥ 1) to be in the palette
wordcloud(sms_corpus_train, min.freq = 30, random.order = FALSE, colors=rainbowLevels)

spam <- subset(sms_raw_train, type == "spam")
ham  <- subset(sms_raw_train, type == "ham")
wordcloud(spam$text, max.words = 40, scale = c(3, 0.5), random.order = FALSE, colors=rainbowLevels)
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5), random.order = FALSE, colors=rainbowLevels)

sms_dict <- findFreqTerms(sms_dtm_train, lowfreq = 5) # 0.1% of no. of documents
sms_train <- DocumentTermMatrix(sms_corpus_train, control = list(dictionary = sms_dict)) # by the argument control list (specify a dictionary), it's a large DocumentTermMatrix
sms_test  <- DocumentTermMatrix(sms_corpus_test, control = list(dictionary = sms_dict)) # ?termFreq and ?WeightFunction, it's a list of 6
# convert counts to a factor
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}
sms_train <- apply(sms_train, MARGIN = 2, convert_counts) # apply can be applied to a DocumentTermMatrix

sms_train[1:10,1:5]

sms_test  <- apply(sms_test, MARGIN = 2, convert_counts) # try sms_test[1:10, 1:10] # a character matrix

library(e1071) # for function 'naiveBayes'
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)
# sms_classifier
names(sms_classifier)
sms_classifier$apriori
sms_classifier$tables[11]
sms_classifier$levels
sms_classifier$call
sms_test_pred <- predict(sms_classifier, sms_test) # It needs some time to make the predictions.
sms_test_pred[1:10]
library(gmodels)
CrossTable(sms_test_pred, sms_raw_test$type, prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, dnn = c('predicted', 'actual')) # FP = 5, FN = 32 (Is it more serious ?)


sms_classifier2 <- naiveBayes(sms_train, sms_raw_train$type, laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test) # It takes some time to make the predictions.
CrossTable(sms_test_pred2, sms_raw_test$type, prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, dnn = c('predicted', 'actual')) # FP: 32 -> 31, FN: 5 -> 3

### 8. k-Nearest Neighbors
# 動畫示例
library(animation)
knn.ani(k=4)
cv.ani()

# import the CSV file
wbcd <- read.csv(file.choose(), stringsAsFactors = FALSE) # select wisc_bc_data.csv
str(wbcd)
# radius: mean of distances from center to points on the perimeter
# texture: standard deviation of gray-scale values
# smoothness: local variation in radius lengths
# compactness: perimeter^2 / area - 1.0
# concavity: severity of concave portions of the contour
# concavity points: number of concave portions of the contour
# fractal dimension: "coastline approximation" - 1
# periment, area, and symmetry

wbcd <- wbcd[-1] # remove id
table(wbcd$diagnosis)
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant"))
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])
wbcd_z <- as.data.frame(scale(wbcd[-1]))
summary(wbcd_z$area_mean)
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ] # the last 100 samples
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]
library(class) # for function 'knn'
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)
library(gmodels)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
} # min-max normalization
normalize(c(1, 2, 3, 4, 5));normalize(c(10, 20, 30, 40, 50))

wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
summary(wbcd_n$area_mean)
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=1)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=5)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=11)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=15)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=27)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

### 9. Neural networks and support vector machine
# NN
concrete <- read.csv("c://R/r.data/concrete.csv")
str(concrete)
normalize <- function(x) {
return((x - min(x)) / (max(x) - min(x)))
}
# apply normalization to entire data frame
concrete_norm <- as.data.frame(lapply(concrete, normalize))
summary(concrete_norm$strength)
summary(concrete$strength)
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]
library(neuralnet)
concrete_model <- neuralnet(formula = strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age, data = concrete_train)
plot(concrete_model)
model_results <- compute(concrete_model, concrete_test[1:8])
predicted_strength <- model_results$net.result
cor(predicted_strength, concrete_test$strength)
concrete_model2 <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age, data = concrete_train, hidden = 5)
plot(concrete_model2)
model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)

# SVM
set.seed(1)
x = matrix(rnorm(20*2), ncol=2)
y = c(rep(-1 ,10), rep(1 ,10))
x[y==1,] = x[y==1,] + 1
plot(x, col=(3 - y))
dat = data.frame(x=x , y=as.factor(y))
library(e1071)
svmfit = svm (y~., data=dat, kernel="linear", cost=10, scale=FALSE)
plot(svmfit, dat)
svmfit$index
summary(svmfit)
svmfit = svm(y~., data=dat, kernel="linear", cost=0.1, scale=FALSE)
plot(svmfit, dat)
svmfit$index
set.seed(1)
tune.out = tune(svm, y~., data=dat, kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)
bestmod = tune.out$best.model
summary(bestmod)
xtest = matrix(rnorm(20*2), ncol=2)
ytest = sample(c(-1, 1), 20, rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,] + 1
testdat = data.frame(x = xtest, y = as.factor(ytest))
ypred = predict(bestmod, testdat)
table(predict=ypred, truth=testdat$y)

# SVM: Case study of letterdata.csv
letters <- read.csv("c://R/r.data/letterdata.csv")
str(letters)
letters_train <- letters[1:16000, ]
letters_test  <- letters[16001:20000, ]
library(kernlab)
letter_classifier <- ksvm(letter ~ ., data = letters_train, kernel = "vanilladot")
letter_classifier
letter_predictions <- predict(letter_classifier, letters_test)
head(letter_predictions)
table(letter_predictions, letters_test$letter)[1:5,1:5]
agreement <- letter_predictions == letters_test$letter
table(agreement)
prop.table(table(agreement))
letter_classifier_rbf <- ksvm(letter ~ ., data = letters_train, kernel = "rbfdot")
letter_predictions_rbf <- predict(letter_classifier_rbf, letters_test)
agreement_rbf <- letter_predictions_rbf == letters_test$letter
table(agreement_rbf)
prop.table(table(agreement_rbf))


###### Please enjoy the world of R. #####