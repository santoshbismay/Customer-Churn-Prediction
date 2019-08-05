
## Importing packages
library(tidyverse) 
library(MASS)
library(car)
library(caret)
library(cowplot)
library(caTools)
library(pROC)
library(ggcorrplot)
library(tidyr)
library(dplyr)
library(ggplot2)
install.packages('sjPlot')

telco <- read.csv("telco_customer_churn.csv")

head(telco)
summary(telco)
missing<- telco %>% summarize_all(funs(sum(is.na(.))))
missing %>% gather(key ="variables", value ="missing_values") %>%
  ggplot(aes(x=variables, y=missing_values)) +geom_bar(stat="identity", fill = "blue") + 
  coord_flip() + ggtitle("Missing values per variable")


##Churn Percent
telco %>% 
  group_by(Churn) %>% 
  summarise(Count = n())%>% 
  mutate(percent = prop.table(Count)*100)%>%
  ggplot(aes(reorder(Churn, -percent), percent), fill = Churn)+
  geom_col(fill = c("#FC4E07", "#E7B800"))+
  geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.01,vjust = -0.5, size =3)+ 
  theme_bw()+
  ggtitle("Customer Churn Percent")+
  xlab("Churn") + 
  ylab("Percent")

corrplot()
  
## Demographics Plot
plot_grid(ggplot(telco, aes(x=gender,fill=Churn))+ geom_bar(),
          ggplot(telco, aes(x=SeniorCitizen,fill=Churn))+ geom_bar(position = 'fill'),
          ggplot(telco, aes(x=Partner,fill=Churn))+ geom_bar(position = 'fill'),
          ggplot(telco, aes(x=Dependents,fill=Churn))+ geom_bar(position = 'fill'),
          ggplot(telco, aes(x=PhoneService,fill=Churn))+ geom_bar(position = 'fill'),
          ggplot(telco, aes(x=MultipleLines,fill=Churn))+ geom_bar(position = 'fill')+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          align = "h")

## Online Services 
plot_grid(ggplot(telco, aes(x=InternetService,fill=Churn))+ geom_bar(position = 'fill')+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)), 
          ggplot(telco, aes(x=OnlineSecurity,fill=Churn))+ geom_bar(position = 'fill') +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(telco, aes(x=OnlineBackup,fill=Churn))+ geom_bar(position = 'fill') +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(telco, aes(x=DeviceProtection,fill=Churn))+ geom_bar(position = 'fill') +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(telco, aes(x=TechSupport,fill=Churn))+ geom_bar(position = 'fill') +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(telco, aes(x=StreamingTV,fill=Churn))+ geom_bar(position = 'fill')+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          align = "h")

## Dataset Cleaning 
telco <- data.frame(lapply(telco, function(x) {
  gsub("No internet service", "No", x)}))

telco <- data.frame(lapply(telco, function(x) {
  gsub("No phone service", "No", x)}))


## Standardizing Continous vatriables
num_columns <- c("tenure", "MonthlyCharges", "TotalCharges")
telco[num_columns] <- sapply(telco[num_columns], as.numeric)

telco_int <- telco[,c("tenure", "MonthlyCharges", "TotalCharges")]
telco_int <- data.frame(scale(telco_int))

## Derived Variables 

telco <- mutate(telco, tenure_bin = tenure)

telco$tenure_bin[telco$tenure_bin >=0 & telco$tenure_bin <= 12] <- '0-1 year'
telco$tenure_bin[telco$tenure_bin > 12 & telco$tenure_bin <= 24] <- '1-2 years'
telco$tenure_bin[telco$tenure_bin > 24 & telco$tenure_bin <= 36] <- '2-3 years'
telco$tenure_bin[telco$tenure_bin > 36 & telco$tenure_bin <= 48] <- '3-4 years'
telco$tenure_bin[telco$tenure_bin > 48 & telco$tenure_bin <= 60] <- '4-5 years'
telco$tenure_bin[telco$tenure_bin > 60 & telco$tenure_bin <= 72] <- '5-6 years'

telco$tenure_bin <- as.factor(telco$tenure_bin)

## Creating Dummy Variable 
telco_cat <- telco[,-c(1,6,19,20)]

#Creating Dummy Variables
dummy<- data.frame(sapply(telco_cat,function(x) data.frame(model.matrix(~x-1,data =telco_cat))[,-1]))
head(dummy)

## Combining the Data 

telco_final <- cbind(telco_int,dummy)
head(telco_final)
str(telco_final)
summary(telco_final)
telco_final <- na.omit(telco_final)
write.csv(telco_final, "telco_final.csv")

##Data Splitting 
telco_final <- read.csv("telco_final.csv")
set.seed(12933625)
indices = sample.split(telco_final$Churn, SplitRatio = 0.7)
train = telco_final[indices,]
validation = telco_final[!(indices),]

## Logistic Regression full model
Telco_logit <- glm(Churn ~., data = train, family = "binomial")
summary(Telco_logit)

##Model Fitting Criteria 
Telco_logit$deviance 
AIC(Telco_logit)
BIC(Telco_logit)
pred_Telco_logit <- predict(Telco_logit, newdata = train, type = "response")

## BIC = 4397.523
##ROC curve
install.packages("ROCR")
library(ROCR)

##In sample
pred <- prediction(pred_Telco_logit, train$Churn)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)

## AUC full model
unlist(slot(performance(pred, "auc"), "y.values"))

## Grid Search 
costfunc = function(obs, pred.p, pcut){
  weight1 = 5   # define the weight for "true=1 but pred=0" (FN)
  weight0 = 1    # define the weight for "true=0 but pred=1" (FP)
  c1 = (obs==1)&(pred.p<pcut)    # count for "true=1 but pred=0"   (FN)
  c0 = (obs==0)&(pred.p>=pcut)   # count for "true=0 but pred=1"   (FP)
  cost = mean(weight1*c1 + weight0*c0)  # misclassification with weight
  return(cost) # you have to return to a value when you write R functions
}

p.seq = seq(0.01, 1, 0.01) 

cost = rep(0, length(p.seq))  
for(i in 1:length(p.seq)){ 
  cost[i] = costfunc(obs = train$Churn, pred.p = pred_Telco_logit, pcut = p.seq[i])  
}

plot(p.seq, cost)


optimal.pcut.glm0 = p.seq[which(cost==min(cost))]
##Cut off Prob = 0.18
class.glm0.train.opt <- (pred_Telco_logit>optimal.pcut.glm0)*1

table(train$Churn, class.glm0.train.opt, dnn = c("True", "Predicted"))

##Out of sample
pred_Telco_logit_test <- predict(Telco_logit,newdata = validation, type = "response")

pred <- prediction(pred_Telco_logit_test, validation$Churn)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)
unlist(slot(performance(pred, "auc"), "y.values"))
## AUC 0.8253
optimal.pcut.glm0 = p.seq[which(cost==min(cost))]

class.glm0.test.opt <- (pred_Telco_logit_test>optimal.pcut.glm0)*1

table(validation$Churn, class.glm0.test.opt, dnn = c("True", "Predicted"))

## Step wise selection procedures
Telco_logit_back <- step(Telco_logit) # backward selection (if you don't specify anything)
summary(Telco_logit_back)
Telco_logit_back$deviance
AIC(Telco_logit_back)
BIC(Telco_logit_back)

##BIC 4349.12

## BIC stepwise
Telco_logit_back_BIC <- step(Telco_logit,k=log(nrow(train))) # backward selection (if you don't specify anything)
summary(Telco_logit_back_BIC)
Telco_logit_back_BIC$deviance
AIC(Telco_logit_back_BIC)
BIC(Telco_logit_back_BIC)

pred_Telco_logit_back_BIC <- predict(Telco_logit_back_BIC, newdata = train, type = "response")

##BIC Model IN sample
pred <- prediction(pred_Telco_logit_back_BIC, train$Churn)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)

## AUC full model
unlist(slot(performance(pred, "auc"), "y.values"))

class.glm0.train.opt <- (pred_Telco_logit_back_BIC>optimal.pcut.glm0)*1

table(train$Churn, class.glm0.train.opt, dnn = c("True", "Predicted"))

##BIC model Out of sample
pred_Telco_logit_back_BIC_test <- predict(Telco_logit_back_BIC, newdata = validation, type = "response")
pred <- prediction(pred_Telco_logit_back_BIC_test, validation$Churn)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)

unlist(slot(performance(pred, "auc"), "y.values"))

class.glm0.train.opt <- (pred_Telco_logit_back_BIC_test>optimal.pcut.glm0)*1

table(validation$Churn, class.glm0.train.opt, dnn = c("True", "Predicted"))

### Classification tree
install.packages('rpart')
install.packages('rpart.plot') 
library(rpart)
library(rpart.plot)

telco.rpart0 <- rpart(formula = Churn ~ ., data = train, method = "class")

pred0<- predict(telco.rpart0, type="class")

table(train$Churn, pred0, dnn = c("True", "Pred"))

prp(telco.rpart0, extra = 1)

##Out of sample
pred0.test <- predict(telco.rpart0, validation , type="class")

table(train$Churn, pred0, dnn = c("True", "Pred"))

##Insample ROC

telco.train.prob.rpart = predict(telco.rpart0,train, type="prob")
pred = prediction(telco.train.prob.rpart[,2], train$Churn)
perf = performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)

slot(performance(pred, "auc"), "y.values")[[1]]

telco.train.pred.rpart = as.numeric(telco.train.prob.rpart[,2] > 0.18)
table(train$Churn, telco.train.pred.rpart, dnn=c("Truth","Predicted"))

## Out of sample

telco.test.prob.rpart = predict(telco.rpart0,validation, type="prob")
pred = prediction(telco.test.prob.rpart[,2], validation$Churn)
perf = performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)

slot(performance(pred, "auc"), "y.values")[[1]]

telco.test.pred.rpart = as.numeric(telco.test.prob.rpart[,2] > 0.18)
table(validation$Churn, telco.test.pred.rpart, dnn=c("Truth","Predicted"))

##Large Tree

telco.rpart.largetree <- rpart(formula = Churn ~ ., data = train, cp= 0.001)

prp(telco.rpart.largetree, extra = 1)

plotcp(telco.rpart.largetree)

## Pruned Treee
telco.rpart.prunedtree <- rpart(formula = Churn ~ ., data = train, cp= 0.012, method = "class")
prp(telco.rpart.prunedtree, extra = 1)

##in sample
pred_in = predict(telco.rpart.prunedtree, train, type="prob")
pred = prediction(pred_in[,2], train$Churn)
perf = performance(pred, "tpr", "fpr")
plot(perf, color = "blue")

slot(performance(pred, "auc"), "y.values")[[1]]

## Out of sample
pred_in = predict(telco.rpart.prunedtree, validation, type="prob")
pred = prediction(pred_in[,2], validation$Churn)
perf = performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE)

slot(performance(pred, "auc"), "y.values")[[1]]

telco.test.pred.rpart.1 = as.numeric(pred_in[,2] > 0.18)
table(validation$Churn, telco.test.pred.rpart.1, dnn=c("Truth","Predicted"))


#### Random Forest
library(randomForest)
telco.rf <- randomForest(as.factor(Churn) ~ ., data = train)
plot(telco.rf, lwd=rep(2, 3))
getTree(telco.rf)
legend("right", legend = c("OOB Error", "FPR", "FNR"), lwd=rep(2, 3), lty = c(1,2,3), col = c("black", "red", "green"))

## Insample
telco.rf.pred.train <- predict(telco.rf, type = "prob")[,2]
pred <- prediction(telco.rf.pred.train, train$Churn)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE) 
unlist(slot(performance(pred, "auc"), "y.values"))

##Out of sample
telco.rf.pred.test <- predict(telco.rf, newdata= validation, type = "prob")[,2]
telco.rf.class.test<- (telco.rf.pred.test>0.18)*1
table(validation$Churn, telco.rf.class.test, dnn = c("True", "Pred"))

telco.rf.pred.test <- predict(telco.rf, validation,type = "prob")[,2]
pred <- prediction(telco.rf.pred.test, validation$Churn)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=TRUE) 
unlist(slot(performance(pred, "auc"), "y.values"))

##############################################
library(ggplot2)
NoChurn <- abc[,1]
Churn <- abc[,2]
group <- c(rep("NoChurn", length(NoChurn)), rep("Churn", length(Churn)))
dat <- data.frame(KSD = c(NoChurn,Churn), group = group)

# create ECDF of data
cdf1 <- ecdf(NoChurn) 
cdf2 <- ecdf(Churn) 
# find min and max statistics to draw line between points of greatest distance
minMax <- seq(min(NoChurn, Churn), max(NoChurn, Churn), length.out=length(NoChurn)) 
x0 <- minMax[which( abs(cdf1(minMax) - cdf2(minMax)) == max(abs(cdf1(minMax) - cdf2(minMax))) )] 
y0 <- cdf1(x0) 
y1 <- cdf2(x0) 

ggplot(dat, aes(x = KSD, group = group, color = group))+
  stat_ecdf(size=1) +
  theme_bw() +
  theme(legend.position ="top") +
  xlab("Churn") +
  ylab("ECDF") +
  #geom_line(size=1) +
  geom_segment(aes(x = x0[1], y = y0[1], xend = x0[1], yend = y1[1]),
               linetype = "dashed", color = "red") +
  geom_point(aes(x = x0[1] , y= y0[1]), color="red") +
  geom_point(aes(x = x0[1] , y= y1[1]), color="red") +
  ggtitle("K-S Test: Customer Churn") +
  theme(legend.title=element_blank())

##################################################

## KS curve 
ks=max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])
plot(perf,main=paste0(' KS=',round(ks*100,1),'%'))
lines(x = c(0,1),y=c(0,1))
print(ks); #0.35

##Gain chart
require(ROCR)
pred<-prediction(telco.rf.class.test, telco.rf.class.test)
gain <- performance(pred, "tpr", "rpp")
plot(gain, main = "Gain Chart")

plot(performance(pred, measure="lift", x.measure="rpp"), colorize=TRUE)

## Lift
lift <- function(depvar, predcol, groups=10) {
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)}
  if(is.factor(depvar)) depvar <- as.integer(as.character(depvar))
  if(is.factor(predcol)) predcol <- as.integer(as.character(predcol))
  helper = data.frame(cbind(depvar, predcol))
  helper[,"bucket"] = ntile(-helper[,"predcol"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(depvar), funs(total = n(),
                                    totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

dt = lift(validation$Churn ,telco.rf.pred.test , groups = 10)

graphics::plot(dt$bucket, dt$Cumlift, main = "lift Chart", type="l", ylab="Cumulative lift", xlab="Bucket", ylim = c(0,4), col = "blue", lwd = 3)
lines(x = c(0,10),y=c(1,1), col = "red", lty = 2 , lwd = 3)

library("e1071")
# Fit Support Vector Machine model to data set

svmfit <- svm(Churn~., data = train, kernel = "linear",cost=10,gamma= 1/length(train), probability = T)

# Create a table of misclassified observations

ypred <- predict(svmfit, train, probability=TRUE)

#Train
ypred_<-attr(ypred, "probabilities")[,2]
ypred1<-(ypred_ >= 0.18)*1
misclass <- table(predict = ypred1, truth = train$Churn)
misclass

#Validation
ypred <- predict(svmfit, validation, probability=TRUE)
ypred_<-attr(ypred, "probabilities")[,2]
ypred1<-(ypred_ >= 0.18)*1
misclass <- table(predict = ypred1, truth = validation$Churn)
misclass

#Gain Chart
summary(telco_final)

require(ROCR)
pred<-prediction(ypred_, ypred1)
gain <- performance(pred, "tpr", "rpp")
plot(gain, main = "Gain Chart")

#Lift Curve

library(ROCR)
plot(performance(pred, measure="lift", x.measure="rpp"), colorize=TRUE)
