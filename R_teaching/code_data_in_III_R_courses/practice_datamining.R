######## Practice 2 ##########

library(arules)# for dataset "Adult" & functions
data(AdultUCI) # the "Census Income" Database
help(AdultUCI)
# dim(AdultUCI)
str(AdultUCI)
AdultUCI[1:3,] #head(AdultUCI, 3)
AdultUCI[["fnlwgt"]] <- NULL  #其時就是移除該變數
#AdultUCI[["fnlwgt]] 其實就是 AdultUCI$fnlwgt <- NULL
#age 是定量變數, 須因子化
summary(AdultUCI$age)

AdultUCI$age <- ordered(cut(AdultUCI$age, c(15,25,45,65,100)),labels = c("Young", "Middle-aged", "Senior", "Old")) #min age is 17

AdultUCI[["hours-per-week"]] <- ordered(cut(AdultUCI[["hours-per-week"]], c(0,25,40,60,168)),labels = c("Part-time", "Full-time", "Over-time", "Workaholic"))

median(AdultUCI[["capital-gain"]][AdultUCI[["capital-gain"]]>0])

AdultUCI[["capital-gain"]]<- ordered(cut(AudltUCI[["capital-gain"]][AdultUCI[["capital-gain"]]>0]), Inf), labels=c("None", "Low", "High")) # the median is 7298
                                                                                                         )
AudltUCI[["capital-loss"]]<- ordered(cut(AdultUCI[["capital-loss"]][AudltUCI[["capital-loss"]]>0]), Inf), labels=c("None", "Low", "High"))

summary(AdultUCI)
Adult <- as(AdultUCI, "transactions")

######## Practise 3 #########
library(DMwR)
data(algae)
str(algae)
help(algae)
help(outliers.ranking)
## Trying to obtain a reanking of the 200 samples
o <- outliers.ranking(algae[,-(1:3)])
o$rank.ouliers
o$rank.ouliers[1:5]
o$prob.ouliers
sort(o$prob.outliers)
sort(o$prob.outliers, decreasing=TRUE, index.return=TRUE) #Six is same as rank.outlies

########### Practice 4###### 沒抄完~~~~~~~~~~~
library(DMnR)
str(algae)
algae[manyNAs(algae),])
algae <- algae[-manyNAs(algae),]

library(rpart) # for function rpart

rt.a1 <- 
  
prettyTree(rt.a1) # {DMnr}
printcp(rt.a1)
rt2.a1<- prune(rt.1,cp=0.08) #min, xerror+xstd=?+?=?, so rel error (tree?)=?<?, its cp is?
rt2.a1
prettyTree(rt2.a1)

#print(bodyfat_rpart$cptable)
#opt <- which.min(bodyfat_rpart$cptable[,"xerror])



