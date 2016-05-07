############################################################
# Collated by Prof. Ching-Shih Tsou (Ph.D.) at the IDS, NTUB(國立臺北商業大學資訊與決策科學研究所); CARS(中華R軟體學會), DSBA(台灣資料科學與商業應用協會)
############################################################

##### Just a little bit review about vector and list creation
patientID <- c(1, 2, 3, 4)
age <- c(25, 34, 28, 52)

c(patientID, age)

diabetes <- c("Type1", "Type2", "Type1", "Type1")
diabetes <- factor(diabetes)
unclass(diabetes)

c(diabetes, age)

status <- c("Poor", "Improved", "Excellent", "Poor")

status <- factor(status, ordered=T, levels=c("Excellent","Improved","Poor"))
status
patientdata <- data.frame(patientID, age, diabetes, status)
str(patientdata)
summary(patientdata)

g <- "My First List"
h <- c(25, 26, 18, 39)
j <- matrix(1:10, nrow=5, byrow=T)
j
k <- c("one", "two", "three")
mylist <- list(title=g, ages=h, j, k)
mylist
mylist[2] # it's also a list
mylist[[2]] # it's a vector
mylist[["ages"]]
mylist$ages

### Slides Codes start from here !!!
# install.packages('DMwR')
library(DMwR)
library(help=DMwR)

### Data Understanding
data(algae)
algae$mxPH
algae[,4]

# 數據結構
str(algae) # compactly display the structure of an arbitrary R object

# 看前幾筆數據
head(algae) # return the first part of an object
# 看最末幾筆數據
tail(algae) # return the last part of an object

# 檢視說明文件
help(algae) # the primary interface to the help systems (R documentation)

# 檢視資料集屬性
attributes(algae) # access an object's attributes

# 檢視變數名稱
names(algae) # get or set the names of an object

# 檢視資料物件類型
class(algae) # return object classes

# 變數個數
length(algae) # get or set the length of vectors (including lists) and factors

# 資料框行列維度
dim(algae) # retrieve or set the dimension of an object

# 檢視摘要統計值
summary(algae) # a generic function used to produce object summaries

### Data Cleaning
is.na(algae$mxPH)
which(is.na(algae$mxPH)) # which one is true?
mxPH.na.omit <- na.omit(algae$mxPH) # try mxPH.na.omit
length(mxPH.na.omit)
attr(mxPH.na.omit, "na.action")
na.fail(algae$mxPH)
complete.cases(algae) # a logical vector of n.obs
which(!complete.cases(algae)) # who are the 16 incomplete cases ?

algae[!complete.cases(algae),]

algae[which(!complete.cases(algae)),] # take a look at the 16 incomplete cases
algae1 <- algae[complete.cases(algae),] # remove them from algae

apply(matrix(1:10, ncol=2), 1, mean)

apply(algae, 1, function(x) sum(is.na(x))) # count the number of missing variables for each observation

options(digits=2)
(Student <- c("John Davis", "Angela Williams", "Bullwinkle Moose", "David Jones", "Janice Markhammer", "Cheryl Cushing", "Reuven Ytzrhak", "Greg Knox", "Joel England", "Mary Rayburn"))
(Math <- c(502, 600, 412, 358, 495, 512, 410, 625, 573, 522))
Science <- c(95, 99, 80, 82, 75, 85, 80, 95, 89, 86)
English <- c(25, 22, 18, 15, 20, 28, 15, 30, 27, 18)
(roster <- data.frame(Student, Math, Science, English, stringsAsFactors=FALSE))
z <- scale(roster[,2:4])
apply(z, 2, mean)
apply(z, 2, sd)
score <- apply(z, 1, mean)
(roster <- cbind(roster, score))
(y <- quantile(score, c(.8,.6,.4,.2))) # it's a named numeric vector
roster$grade[score >= y[1]] <- "A" # append a new column to roster
roster$grade[score < y[1] & score >= y[2]] <- "B"
roster$grade[score < y[2] & score >= y[3]] <- "C"
roster$grade[score < y[3] & score >= y[4]] <- "D"
roster$grade[score < y[4]] <- "F"
name <- strsplit((roster$Student), " ")
(lastname <- sapply(name, "[", 2))
(firstname <- sapply(name, "[", 1))
roster <- cbind(firstname,lastname, roster[,-1])
roster <- roster[order(lastname, firstname),]
roster

sort(lastname, firstname) # An error occurs
sort(lastname) # what is the difference between sort and order?
sort(lastname, index.return=T)

x <- data.frame(a=c(1,2,4,5,6), x=c(9,12,14,21,8))
y <- data.frame(a=c(1,3,4,6), y=c(8,14,19,2))
merge(x,y) # natural join
merge(x,y,all=TRUE) # full outer join
merge(x,y,all.x=TRUE) # left outer join
merge(x,y,all.y=TRUE) # right outer join

head(iris, 6)
str(iris)
summary(iris)

library(Hmisc)
describe(iris)

# Be naive by subset
setosa <- subset(iris, Species=='setosa')
versicolor <- subset(iris, Species=='versicolor')
virginica <- subset(iris, Species=='virginica')
# Use a loop
results <- data.frame() # for gathering results
for (species in unique(iris$Species)) { # How smart it is !
  tmp <- subset(iris, Species==species) # get subset here
  count <- nrow(tmp)
  mean <- mean(tmp$Sepal.Length)
  median <- median(tmp$Sepal.Length)
  results <- rbind(results, data.frame(species, count, mean, median))
} #
results
# tapply {base}, input a vector that is grouped by a list of one or more factors

tapply(iris$Sepal.Length, iris$Species, FUN=length)
tapply(iris$Sepal.Length, iris$Species, FUN=mean)
tapply(iris$Sepal.Length, iris$Species, FUN=median) # Can multiple functions be set in FUN? No!!!
cbind(count=tapply(iris$Sepal.Length, iris$Species, FUN=length), mean=tapply(iris$Sepal.Length, iris$Species, FUN=mean), median=tapply(iris$Sepal.Length, iris$Species, FUN=median))

# aggregate {stats}, input a vector, matrix, or data frame that is grouped by a list of one or more factors
aggregate(Sepal.Length~Species, data=iris, FUN='length') # '~' is grouped by 
aggregate(Sepal.Length~Species, data=iris, FUN='mean')
aggregate(Sepal.Length~Species, data=iris, FUN='median') # Can multiple functions be set in FUN?  No!!!

# summaryBy {doBy}, performe multiple operations (functions)
library('doBy')
summaryBy(Sepal.Length~Species, data=iris, FUN=function(x) {c(count=length(x), mean=mean(x), median=median(x))}) # the columns are automatically given

# ddply {plyr}, multiple-part grouping variables and functions
library('plyr')
ddply(iris, 'Species', function(x) c(count=nrow(x), mean=colMeans(x[-5])))
# 

### Data Reshaping
pop <- read.csv('http://www.census.gov/popest/data/national/totals/2012/files/NST-EST2012-popchg2010_2012.csv')
pop <- pop[,c("Name","POPESTIMATE2010","POPESTIMATE2011","POPESTIMATE2012")]
colnames(pop)
colnames(pop) <- c('state', seq(2010, 2012))
head(pop, 2)
library('doBy')
top <- orderBy(~-2010, pop) # decreasing by 2010
head(top,3)
library('reshape2') # for melt() and dcast()
mtop <- melt(top, id.vars='state', variable.name='year', value.name='population') # wide to long
tail(mtop) # easier for analysis, plotting, database storage, etc.
dcast(mtop, state~year, value.var='population')[1:5,] # long to wide (sometimes you really can put the toothpaste back into the tube)

##### Supplements for XML and JSON data
### Reading Data from HTML Tables
library(XML)
u = "http://en.wikipedia.org/wiki/Country_population"
# This HTML document, like many documents on the Web, contains several tables that are used to format ads and other parts of the Web page.
tbls = readHTMLTable(u) # pay attention to a List of five NULLs:'data.frame'


sapply(tbls, nrow) # look at the number of rows in each table, one larger table in posistion one

?readHTMLTable
pop = readHTMLTable(u, which = 1) # some territorial dispute regions

### Reading Data from XML-formatted Documents
system.file("help", "AnIndex", package = "splines") # character vectors, specifying subdirectory and file(s) within some package. The default, none, returns the root of the package. Wildcards are not supported. "/Library/Frameworks/R.framework/Resources/library/splines/help/AnIndex"

fileLoc = system.file("exampleData", "kiva_lender.xml", package = "XML") # /Library/Frameworks/R.framework/Versions/3.1/Resources/library/XML/exampleData/kiva_lender.xml
doc = xmlParse(fileLoc) # .xml In and an XML tree Out


kivaList = xmlToList(doc, addAttributes = FALSE) # an XML tree In and a large list of 2 elements Out: header (List of 4) and lenders (List of 1000)
names(kivaList) # LOOK AT THE NAMES BY LIST
kivaList$lenders[1]
kivaList$lenders[[1]]

xmlRoot(doc) # an XML tree In and the top-level XML node Out

names(xmlRoot(doc)) # LOOK AT THE NAMES BY xmlRoot, it gives us the top-level node of our document, i.e., <snapshot>, it has two subnodes - "header" and "lenders"
lendersNode = xmlRoot(doc)[["lenders"]]
class(lendersNode) # "XMLInternalElementNode" "XMLInternalNode" "XMLAbstractNode"

xmlRoot(doc)[[2]] # identical(xmlRoot(doc)[["lenders"]], xmlRoot(doc)[[2]])


lenders = xmlToDataFrame(xmlChildren(lendersNode))


names(lenders)

# The above example introduced two high-level functions: xmlToList and xmlToDataFrame. We also got a glimpse of xmlParse, xmlRoot, xmlChildren, and '[['

### Reading Data from JSON-formatted Documents (Java Script Object Notation)
library(RJSONIO)
kiva = fromJSON("/Users/vince/cstsouMac/RandS/Rexamples/XML and Web Technologies for Data Sciences with R/1.json") # .json In and a large list Out


names(kiva)


kiva$lenders[[1]] # the data of the first lender
######################################################

id <- c("mobile phone", "food & beverage", "computer & office", "homes", "mother, baby & toys", "appliance", "clothes & shoes", "varieties", "virtual product", "gifts")
conver <- c(0.012,0.02,0.015,0.014,0.018,0.013,0.01,0.025,0.045,0.011)
pv <- c(23.19,10.89,15.09,12.11,9.6,20.29,40.56,28.66,20.43,13.84)
price <- c(3509,59,2501,509,411,3011,476,81,379,610)
library(RColorBrewer)
col <- brewer.pal(11, "Spectral")[2:11]
cex.max <- 12
cex.min <- 3
a <- (cex.max-cex.min)/(max(price)-min(price))
b <- cex.min - a*min(price)
cex2 <- a*price + b
plot(pv,conver,col=col,cex=cex2,pch=16,ylim=c(0,0.06),xlab="Page View (ten thousands)",ylab="Conversion Rate",main=list("Conversion Rate - Page View - Price",cex=2),yaxt='n')
legend("topleft",legend=id,pch=16,col=col,bty='n',cex=0.8,ncol=3)
axis(2,labels=paste(seq(0,5,1),"%",sep=""),at=seq(0,0.05,0.01))
text(x=pv,y=conver,labels=price,cex=0.8)
text(x=40,y=0.055,labels="Z-Price",cex=1.3)

# title  : R - basic-iii-day2
# date   : 2014.8.3
# author : Ming-Chang Lee
# email  : alan9956@gmail.com
# RWEPA  : http://rwepa.blogspot.tw/

# Simple linear regression
?lm
# my.lm <- lm(formula, data="xxx")
# formula: y ~ x1 + x2 + ... +xn, actually a multiple regression
# end

# women: Average Heights and Weights for American Women
# y: weight
# x: height
fit.lm <- lm(weight ~ height, data=women) # '~' stands for weight is modeled by height
class(fit.lm)
summary(fit.lm)
# weight = -87.52+3.45*height

# verify residuals
names(fit.lm)
women$weight   # actual
fitted(fit.lm) # predicted
residuals(fit.lm) # residual=actual-predicted
women$weight - fitted(fit.lm)

# plot data
plot(women$height,women$weight, xlab="Height (in inches)", ylab="Weight (in pounds)", main="Average Heights and Weights for American Women")
abline(fit.lm, col="red")
# end

# Polynomial regression
fit.poly.lm <- lm(weight ~ height + I(height^2), data=women)
summary(fit.poly.lm)
# weight = 261.88 - 7.35*height + 0.083*height^2

# plot data with polynomial regression
plot(women$height,women$weight, main="Polynomial regression", xlab="Height (in inches)", ylab="Weight (in lbs)")
 lines(women$height, fitted(fit.poly.lm), col="blue")
# end

# cubic polynomial regression
fit.cubic.lm <- lm(weight ~ height + I(height^2) +I(height^3), data=women)
summary(fit.cubic.lm)

# plot data with cubic polynomial regression
plot(women$height,women$weight, main="Cubic polynomial regression", xlab="Height (in inches)", ylab="Weight (in lbs)")
lines(women$height, fitted(fit.cubic.lm), col="blue")
# end

# scatterplot{car}
library(car)
scatterplot(weight ~ height, data=women, pch=19, spread=FALSE,
            lty=2, # lty=2, dashed line in linear model
            main="Women Age 30-39",
            xlab="Height (inches)",
            ylab="Weight (lbs.)")
# end

# compare of lm, Resistant, robust
library(MASS) # for lqs()
library(nutshell) # for data shiller.index
data(shiller) # nutshell package
hpi.lm <- lm(Real.Home.Price.Index~Year, data=shiller.index)
hpi.rlm <- rlm(Real.Home.Price.Index~Year, data=shiller.index)
hpi.lqs <- lqs(Real.Home.Price.Index~Year, data=shiller.index)
plot(Real.Home.Price.Index~Year, pch=19, cex=0.3, data=shiller.index)
abline(reg=hpi.lm, lty=1)
abline(reg=hpi.rlm, lty=2, col="red")
abline(reg=hpi.lqs, lty=3, col="green")
legend(x=1900, y=200, legend=c("lm", "rlm", "lqs"), lty=c(1, 2, 3), col=c(1,2,3))

# nonlinear regression
# example - population growth
library(car)
data(USPop)
attach(USPop)
plot(year, population)

# nls demo
time <- 0:21
pop.mod <- nls(population ~ beta1/(1 + exp(beta2 + beta3*time)),
               start=list(beta1 = 350, beta2 = 4.5, beta3 = -0.3),
               trace=TRUE)
summary(pop.mod)

lines(year, fitted.values(pop.mod), lwd=2, col="red")

plot(year, residuals(pop.mod), type="b") # type="b" means that plotting points and line both
abline(h=0, lty=2) # low-level plooting

# Local Polynomial Regression)
library(locfit)

OldFaithful <- read.csv("OldFaithful.csv")
OldFaithful[1:3,]

## density histograms and smoothed density histograms
## time of eruption
hist(OldFaithful$TimeEruption,freq=FALSE)
fit1 <- locfit(~lp(TimeEruption),data=OldFaithful)
plot(fit1)

## waiting time to next eruption
hist(OldFaithful$TimeWaiting,freq=FALSE)
fit2 <- locfit(~lp(TimeWaiting),data=OldFaithful)
plot(fit2)


## general cross validation (GCV) of smoothing constant
## for waiting time to next eruption
alpha <- seq(0.20,1,by=0.01)
n1 <- length(alpha)
g <- matrix(nrow=n1,ncol=4)
for (k in 1:length(alpha)) {
  g[k,] <- gcv(~lp(TimeWaiting,nn=alpha[k]),data=OldFaithful)
}
g # nn=0.66

plot(g[,4]~g[,3],ylab="GCV",xlab="degrees of freedom")
abline(h=min(g[,4]), lty=4, col="red")
## minimum at nn = 0.66
fit2 <- locfit(~lp(TimeWaiting,nn=0.66,deg=2),
               data=OldFaithful)
plot(fit2)

# polynominlal vs.regression
fitreg <- lm(TimeWaiting~TimeEruption,data=OldFaithful)
fit3 <- locfit(TimeWaiting~lp(TimeEruption),data=OldFaithful) # deg=2
plot(fit3, get.data=T)
abline(fitreg, col="red")
legend("topleft",legend=c("polynominlal","regression"),lty=1,col=c(1,2),inset=0.05)

# anova
grader1 <- c(4,3,4,5,2,3,4,5)
grader2 <- c(4,4,5,5,4,5,4,4)
grader3 <- c(3,4,2,4,5,5,4,4)
scores <- data.frame(grader1, grader2, grader3)
boxplot(scores)

scores <- stack(scores)
oneway.test(values ~ ind, data=scores, var.equal=T)

anova(lm(values ~ ind, data=scores))

##### Supplements of Regression
## Example: Predicting Medical Expenses ----
## Step 2: Exploring and preparing the data ----
insurance <- read.csv("~/cstsouMac/RandS/Rexamples/MLwithR/insurance.csv", stringsAsFactors = TRUE)
str(insurance)

# summarize the charges variable
summary(insurance$charges)

# histogram of insurance charges
hist(insurance$charges)

# table of region
table(insurance$region)

# exploring relationships among features: correlation matrix
cor(insurance[c("age", "bmi", "children", "charges")])

# visualing relationships among features: scatterplot matrix
pairs(insurance[c("age", "bmi", "children", "charges")]) # pairs {graphics}

# more informative scatterplot matrix
library(psych)
pairs.panels(insurance[c("age", "bmi", "children", "charges")])

## Step 3: Training a model on the data ----
ins_model <- lm(charges ~ age + children + bmi + sex + smoker + region, data = insurance)
ins_model <- lm(charges ~ ., data = insurance) # this is equivalent to above

# see the estimated beta coefficients
ins_model

## Step 4: Evaluating model performance ----
# see more detail about the estimated beta coefficients
summary(ins_model)

## Step 5: Improving model performance ----

# add a higher-order "age" term
insurance$age2 <- insurance$age^2

# add an indicator for BMI >= 30
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)

# create final model
ins_model2 <- lm(charges ~ age + age2 + children + bmi + sex +
                   bmi30*smoker + region, data = insurance)

summary(ins_model2)

##### Supplements for Regression
library(nutshell)
data(team.batting.00to08)
help(team.batting.00to08) # ? is an alias of help()
str(team.batting.00to08)

library(nutshell)
data(team.batting.00to08)
runs.mdl <- lm(formula=runs ~ singles+doubles+triples+homeruns+walks+hitbypitch+sacrificeflies+ stolenbases+caughtstealing, data=team.batting.00to08)
class(runs.mdl)
summary(runs.mdl) # a generic function, actully calls the summary.lm(...)
names(runs.mdl)
runs.mdl$coefficients # 10 coefficients
runs.mdl$residuals # 270 residuals
runs.mdl$effects # 4*68+2=274
runs.mdl$rank # 1 (intercept) + 9 (indep. var.) = 10 秩
runs.mdl$fitted.values # 270 fitted values
runs.mdl$assign # 0 1 2 3 4 5 6 7 8 9
runs.mdl$qr # 270 * 10
runs.mdl$df.residual # 260
runs.mdl$xlevels # a record of the levels of the factors used in fitting
runs.mdl$call
runs.mdl$terms # (1+9) * 9 with 0 and 1 entries
runs.mdl$model # 270 * 10, the model frame used (raw data)
op <- par(mfrow=c(2,2))
plot(runs.mdl) # call plot.lm
par(op)
# model fit (residuals) diagnostics
op <- par(mfrow=c(2,3))
plot(runs.mdl, which=1:6) # a generic function, actully calls the plot.lm(...)
par(op)

confint(runs.mdl)

# selecting the best regression variables
reduced.runs.mdl <- step(runs.mdl, direction='backward') # the smaller the AIC or BIC, the better the fit

min.runs.mdl <- lm(runs ~ 1, data=team.batting.00to08) # the minimum model
fwd.runs.mdl <- step(min.runs.mdl, direction='forward', scope=( ~ singles+doubles+triples+homeruns+walks+hitbypitch+sacrificeflies+stolenbases+caughtstealing), trace=0)
summary(fwd.runs.mdl)

# ANOVA statistics for the fitted model
anova(runs.mdl)
anova(fwd.runs.mdl, runs.mdl) # Akaike's An Information Criterion (-2*log-likehood + k*npar, k=2), k=log(n) for BIC or SBC (Schwarz's Bayesian criterion), the smaller the AIC or BIC, the better the fit, but there is not statistically significant ! 

# predicting
dreamTeam <- data.frame(homeruns=270, singles=1000, doubles=400, walks=600, triples=25, sacrificeflies=55, hitbypitch=44) # creat the new data
dreamTeam
predict(fwd.runs.mdl, newdata=dreamTeam, interval='confidence') # a generic function, actully calls the predict.lm(...)
predict(fwd.runs.mdl, newdata=dreamTeam, interval='prediction') # wider than confidence interval

# refining the model based on previous calculations
runs.mdl2 <- update(runs.mdl, formula=runs ~ singles + doubles + triples + homeruns + walks + hitbypitch + stolenbases + caughtstealing + 0) # or -1
summary(runs.mdl2)

#####Supplements for ANOVA
# Comparing means across more than two groups
data(mort06.smpl)
str(mort06.smpl);help(mort06.smpl)
tapply(mort06.smpl$age, INDEX=list(mort06.smpl$Cause), FUN=summary)

length(which(mort06.smpl$Cause=='Suicide')) # verify the valid N from Deducer and NA's from summary(...)

m <- aov(age~Cause, data=mort06.smpl)
class(m)
summary(m)

# By lm(...)
mort06.smpl.lm <- lm(age~Cause, data=mort06.smpl)
anova(mort06.smpl.lm) # samev as aov

# By oneway.test(...)
?oneway.test
oneway.test(age~Cause, data=mort06.smpl)
oneway.test(age~Cause, data=mort06.smpl, var.equal=TRUE) # there is an argument about var.equal, same as aov and anova

# Multiple comparisons
TukeyHSD(m)
op <- par(mar=c(5.1,8.1,4.1,0.1))
plot(TukeyHSD(m), las=1, cex.lab=.5)
par(op)
# Creating an Interaction Plot
# install.packages('faraway')
library(faraway)
data(rats)
interaction.plot(rats$poison, rats$treat, rats$time)
# Note the error bars produced by Rcmdr is not shown here

data(births2006.smpl)
births2006.cln <- births2006.smpl[births2006.smpl$WTGAIN<99 & !is.na(births2006.smpl$WTGAIN),]
tapply(X=births2006.cln$WTGAIN, INDEX=births2006.cln$DOB_MM, FUN=mean) # the weight gain increases slightly during winter months, but is this difference statistically significant? Let's check it now.
aov(WTGAIN~DOB_MM, births2006.cln)
summary(aov(WTGAIN~DOB_MM, births2006.cln))

oneway.test(WTGAIN~DOB_MM, births2006.cln)

oneway.test(WTGAIN~DOB_MM, births2006.cln, var.equal=TRUE)

#####################################################
# end