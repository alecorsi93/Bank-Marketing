setwd("~/Google Drive/BUSINESS INTELLIGENCE/R/Project")

getmode <- function(x){
  uniqv <- unique(x)
  mode = uniqv[which.max(tabulate(match(x,uniqv)))]
  return(mode)
}

## PACKAGES
library(DataExplorer)
library(ggplot2)
library(corrplot)
library(pander)
library(caret)
library(pROC)
library(rpart)
library(rpart.plot)
library(randomForest)

## DATASET

# load dataset
dataset <- read.table("bank-full.csv", sep = ";", header = T)

#structure of the dataset
str(dataset)

#check if there are any missing values
pander(introduce(dataset))
plot_missing(dataset)

## DATA EXPLORATION

# data preprocessing
dataset$duration <- round(dataset$duration/60,2)

# summary of all the attributes
pander(summary(dataset))

# barplot of all the categorical attributes
plot_bar(dataset, ncol = 4L, ggtheme = theme_bw())

# histogram of all numerical attributes
plot_histogram(dataset, ncol = 4L, ggtheme = theme_bw())

# y
pander(round(prop.table(table(dataset$y))*100,2))

# job
pander(round(prop.table(table(dataset$job))*100,2))
ggplot(dataset, aes(x = job, fill = y)) + 
  geom_bar(position = position_dodge(preserve = "single"))+
  theme_bw()
pander(round(prop.table(table(dataset$job,dataset$y))*100,2))

# marital
pander(round(prop.table(table(dataset$marital))*100,2))
ggplot(dataset, aes(x = marital, fill = y)) + 
  geom_bar(position = position_dodge(preserve = "single")) +
  theme_bw()
pander(round(prop.table(table(dataset$marital,dataset$y))*100,2))

# education
pander(round(prop.table(table(dataset$education))*100,2))
ggplot(dataset, aes(x = education, fill = y)) + 
  geom_bar(position = position_dodge(preserve = "single")) + 
  theme_bw()
pander(round(prop.table(table(dataset$education,dataset$y))*100,2))

# default
pander(round(prop.table(table(dataset$default))*100,2))
ggplot(dataset, aes(x = default, fill = y)) + 
  geom_bar(position = position_dodge(preserve = "single")) + 
  theme_bw()
pander(round(prop.table(table(dataset$default,dataset$y))*100,2))

# housing
pander(round(prop.table(table(dataset$housing))*100,2))
ggplot(dataset, aes(x = housing, fill = y)) + 
  geom_bar(position = position_dodge(preserve = "single")) + 
  theme_bw()
pander(round(prop.table(table(dataset$housing,dataset$y))*100,2))

# loan
pander(round(prop.table(table(dataset$loan))*100,2))
ggplot(dataset, aes(x = loan, fill = y)) + 
  geom_bar(position = position_dodge(preserve = "single")) + 
  theme_bw()
pander(round(prop.table(table(dataset$loan,dataset$y))*100,2))

# contact
pander(round(prop.table(table(dataset$contact))*100,2))
ggplot(dataset, aes(x = contact, fill = y)) + 
  geom_bar(position = position_dodge(preserve = "single")) + 
  theme_bw()
pander(round(prop.table(table(dataset$contact,dataset$y))*100,2))

# month
pander(round(prop.table(table(dataset$month))*100,2))
ggplot(dataset, aes(x = month, fill = y)) + 
  geom_bar(position = position_dodge(preserve = "single")) + 
  theme_bw()
pander(round(prop.table(table(dataset$month,dataset$y))*100,2))

# day
pander(round(prop.table(table(dataset$day))*100,2))
ggplot(dataset, aes(x = day, fill = y)) + 
  geom_bar(position = position_dodge(preserve = "single")) + 
  theme_bw()
pander(round(prop.table(table(dataset$day,dataset$y))*100,2))

# poutcome
pander(round(prop.table(table(dataset$poutcome))*100,2))
ggplot(dataset, aes(x = poutcome, fill = y)) + 
  geom_bar(position = position_dodge(preserve = "single")) + 
  theme_bw()
pander(round(prop.table(table(dataset$poutcome,dataset$y))*100,2))

# age
ggplot(dataset, aes(x = age, fill = y)) + 
  geom_density(alpha = 0.4)+
  theme_bw()

#campaign
pander(round(prop.table(table(dataset$campaign))*100,2))
getmode(dataset$campaign)

# duration: from seconds to minutes
getmode(dataset$duration)
ggplot(dataset,aes(x = duration, fill = y)) +
  geom_density(alpha = 0.4)+
  theme_bw()

# pdays
getmode(dataset$pdays)
ggplot(dataset,aes(x = pdays, fill = y)) +
  geom_density(alpha = 0.4)+
  theme_bw()

# previous
getmode(dataset$previous)
ggplot(dataset,aes(x = previous, fill = y)) +
  geom_density(alpha = 0.4)+
  theme_bw()

# balance
ggplot(dataset, aes(x="",y = balance)) + 
  geom_boxplot()
  theme_bw()
ggplot(dataset, aes(x = balance, fill = y)) +
  geom_density(position = position_dodge(preserve = "single"))+
  theme_bw()

# correlation matrix
dataset_num <- dataset
i <- c(2,3,4,5,7,8,9,11,16)
dataset_num[,i] <- apply(dataset_num[,i],2,function(x) as.numeric(as.factor(x)))
dataset_num$y <- ifelse(dataset_num$y=="no",0,1)
corrplot(cor(dataset_num[,1:17])) 
corrplot(cor(dataset_num),method = "number")

## MODELS

# train and test dataset
set.seed(123) 
index <- createDataPartition(dataset$y,p=0.75,list=FALSE) 
train <- dataset[index,]
test <- dataset[-index,]

# LOGISTIC REGRESSION

# model1
log1 <- glm(y ~ .-1-duration, data = train, family = "binomial")
summary(log1)
test$log1 <- predict(log1, test, type="response")
test$pred1 <- as.factor(ifelse(test$log1 > 0.5, "yes", "no"))
confusionMatrix(test$pred1,test$y, positive="yes")
roc.log1 <- roc(test$y~test$log1, plot=TRUE,print.auc=TRUE,print.auc.y = .4,col= "blue",
                legacy.axes=TRUE,xlab="False positive rate",ylab="True positive rate")

# model2
log2 <- glm(y ~ .-1-duration-age-default-day-pdays, data = train, family = "binomial")
summary(log2)
test$log2 <- predict(log2, test, type="response")
test$pred2 <- as.factor(ifelse(test$log2 > 0.5, "yes", "no"))
confusionMatrix(test$pred2,test$y, positive="yes")
roc.log2 <- roc(test$y~test$log2, plot=TRUE,print.auc=TRUE,print.auc.y = .6,
                col= "red",legacy.axes=TRUE,xlab="False positive rate",ylab="True positive rate",
                add=TRUE)

# change the threshold
coords(roc.log2, "best", ret = "threshold")

# CLASSIFICATION TREE
tree <- rpart(y~.-duration,train)
rpart.plot(tree)
tree.predict <- predict(tree, test, type="class")
confusionMatrix(tree.predict,test$y, positive="yes")

tree1 <- rpart(y~.,train)
rpart.plot(tree1)
tree1.predict <- predict(tree1, test, type="class")
confusionMatrix(tree1.predict,test$y, positive="yes")

# RANDOM FOREST
rf <- randomForest(y~.-duration, train)
rf.predict <- predict(rf,test,type="class")
confusionMatrix(rf.predict,test$y, positive="yes")

rf1 <- randomForest(y~., train)
rf1.predict <- predict(rf1,test,type="class")
confusionMatrix(rf1.predict,test$y, positive="yes")