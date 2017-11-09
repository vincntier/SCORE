#logistic regression
library(MASS)
library(tree)
library(rpart)
library(partykit)
library(rpart.plot)

#Analyse Ken
hotspot.ken <- read.csv("/Users/Vincentier/Desktop/SCORE/dataset/Kenya_Sm2_villages_Y1to5_Persistent_Hotspot_dataset_Oct2017.csv")

#create new variables
hotspot.ken <- hotspot.ken[, c(1,3,4,5,9,10,14,15,20,21,26,27)]
hotspot.ken$prevchg_y1y2 <- (hotspot.ken$Prevalence_Y1-hotspot.ken$Prevalence_Y2)/hotspot.ken$Prevalence_Y1


hotspot.ken$coverchg_y1y2<-(hotspot.ken$Y1_Coverage_SAC-hotspot.ken$Y2_Coverage_SAC)/hotspot.ken$Y1_Coverage_SAC
hotspot.ken$intenchg_y1y2<-(hotspot.ken$Mean_intensity_epg_Overall_Y1-hotspot.ken$Mean_intensity_epg_Overall_Y2)/hotspot.ken$Mean_intensity_epg_Overall_Y1

for (n in 1:150){
if (hotspot.ken$Prevalence_gt200epg_Y1[n] > 0){
  hotspot.ken$gt200chg_y1y2[n]<-(hotspot.ken$Prevalence_gt200epg_Y1[n]-hotspot.ken$Prevalence_gt200epg_Y2[n])/hotspot.ken$Prevalence_gt200epg_Y1[n]
 } 
  else{
   hotspot.ken$gt200chg_y1y2[n] <- NA
 }
  if(hotspot.ken$Prevalence_gt400epg_Y1[n] > 0){
    hotspot.ken$gt400chg_y1y2[n]<-(hotspot.ken$Prevalence_gt400epg_Y1[n]-hotspot.ken$Prevalence_gt400epg_Y2[n])/hotspot.ken$Prevalence_gt400epg_Y1[n]  
  }
  else{
    hotspot.ken$gt400chg_y1y2[n] <- NA
  }
}
  
mydata1 <- hotspot.ken
# mydata1 <- subset(mydata1, mydata1$Prevalence_gt200epg_Y1 !=0 & mydata1$Prevalence_gt400epg_Y1 !=0 & mydata1$Prevalence_gt200epg_Y2 !=0 & mydata1$Prevalence_gt400epg_Y1 !=0)
# mydata <- subset(mydata, mydata$gt400chg_y1y2 != -Inf)
View(mydata1)

#select variables for logistic regression


#check useful variables
 stepAIC(fit.logit, direction = "backward")
 stepAIC(fit.logit, direction = "both")
#select epg_y1 intensityc_y1y2

a1<-0
a2<-0
a3<-0
a4<-0
#mydata1 <- mydata1[, c(1,4, 10)]
#mydata1 <- mydata1[,c(1,2,4)]#15;
for (n in 1:1000) 
  {
mydata1$Persistent_Hotspot <- factor(mydata1$Persistent_Hotspot, levels = c(0,1))
train <- sample(nrow(mydata1), 0.7*nrow(mydata1))
mydata1.train <- mydata1[train,]
mydata1.validate <- mydata1[-train,]

fit.logit <- glm(mydata1.train$Persistent_Hotspot ~ ., data = mydata1.train, family = binomial())
# #summary(fit.logit)
# library(Deducer)
# rocplot(fit.logit)
# 
prob <- predict(fit.logit, mydata1.validate, type = "response")
logit.pred <- factor(prob > 0.83, levels = c(FALSE, TRUE))
logit.perf <- table(mydata1.validate$Persistent_Hotspot, logit.pred, dnn = c("actual", "predicted"))
# logit.perf

a1 <- logit.perf[1,1]+a1
a2 <- logit.perf[1,2]+a2
a3 <- logit.perf[2,1]+a3
a4 <- logit.perf[2,2]+a4
# a1 <- logit.perf[1,1]
# a2 <- logit.perf[1,2]
# a3 <- logit.perf[2,1]
# a4 <- logit.perf[2,2]
}
a1 <- a1/1000
a2 <- a2/1000
a3 <- a3/1000
a4 <- a4/1000
a1a2 <- c(a1,a2)
a3a4 <- c(a3,a4)
a <- rbind(a1a2,a3a4)
performance(a)



b1<-0
b2<-0
b3<-0
b4<-0
for (n in 1:100) 
{
  mydata1$Persistent_Hotspot <- factor(mydata1$Persistent_Hotspot, levels = c(0,1))
  train <- sample(nrow(mydata1), 0.8*nrow(mydata1))
  mydata1.train <- mydata1[train,]
  mydata1.validate <- mydata1[-train,]
  
#rpart
hotspot.ken.rpart <- rpart(mydata1$Persistent_Hotspot~ .,
                           data = mydata1, method = "class", parms = list(split = "information"))
#summary(hotspot.ken.rpart)
# plotcp(hotspot.ken.rpart)
# hotspot.ken.rpart$cptable
# #choose specific cp
#hotspot.ken.rpart.pruned <- prune(hotspot.ken.rpart, cp = 0.037)
plot(hotspot.ken.rpart, uniform = TRUE, compress = TRUE, lty = 3, branch = 0.7)
text(hotspot.ken.rpart,all=TRUE, digits= 7, use.n = TRUE, cex = 0.6, xpd = TRUE)

hotspot.ken.rpart.pred <- predict(hotspot.ken.rpart, mydata1.validate, type = "class")
logit.perf <- table(mydata1.validate$Persistent_Hotspot, hotspot.ken.rpart.pred, dnn = c("actual", "predicted"))
#logit.perf
b1 <- logit.perf[1,1]+b1
b2 <- logit.perf[1,2]+b2
b3 <- logit.perf[2,1]+b3
b4 <- logit.perf[2,2]+b4
# b1 <- logit.perf[1,1]
# b2 <- logit.perf[1,2]
# b3 <- logit.perf[2,1]
# b4 <- logit.perf[2,2]
}

b1 <- b1/100
b2 <- b2/100
b3 <- b3/100
b4 <- b4/100
b1b2 <- c(b1,b2)
b3b4 <- c(b3,b4)
b <- rbind(b1b2,b3b4)

performance(b)







#draw first plot
# hotspot.ken.rpart.perf <- table(mydata1$Persistent_Hotspot, hotspot.ken.rpart.pred, dnn = c("Actual", "Predicted"))
# hotspot.ken.rpart.perf
#draw second plot
plot(hotspot.ken.rpart.pruned, uniform = TRUE, compress = TRUE, lty = 3, branch = 0.7)
text(hotspot.ken.rpart.pruned,all=TRUE, digits= 7, use.n = TRUE, cex = 0.6, xpd = TRUE)
# 
# prp(hotspot.ken.rpart.pruned, type = 2, extra = 104, fallen.leaves = TRUE, main = "Decision Tree")



performance <- function(table, n=2){
  if(!all(dim(table) == c(2,2)))
    stop("Must be a 2 x 2 table")
  tn = table[1,1]
  fp = table[1,2]
  fn = table[2,1]
  tp = table[2,2]
  sensitivity = tp/(tp+fn)
  specificity = tn/(tn+fp)
  ppp = tp/(tp+fp)
  npp = tn/(tn+fn)
  hitrate = (tp+tn)/(tp+tn+fp+fn)
  result <- paste("Sensitivity = ", round(sensitivity, n) ,
                  "\nSpecificity = ", round(specificity, n),
                  "\nPositive Predictive Value = ", round(ppp, n),
                  "\nNegative Predictive Value = ", round(npp, n),
                  "\nAccuracy = ", round(hitrate, n), "\n", sep="")
  cat(result)
}