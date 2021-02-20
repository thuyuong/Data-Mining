

library(ggthemes)
library(ggplot2)
library(caret)
library(randomForest)
library(e1071)
library(grid)
library(gridExtra)
library(mice)
library(dplyr)
library(pscl)
library(rpart)

knitr::opts_chunk$set(echo = TRUE)
#load data
train <- read.csv('C:/Users/uongt/Downloads/titanic/train.csv', na.strings = c("N/A","DIV/0!",""), stringsAsFactors = FALSE)
test  <- read.csv('C:/Users/uongt/Downloads/titanic/test.csv', na.strings = c("N/A","DIV/0!",""), stringsAsFactors = FALSE)
full <- bind_rows(train,test)
dim(full)

full[full==""] <- NA
a<- apply(full,2,is.na)
summary(a)
apply(a,2,sum)
#lay title tu ten hk
full$Title<- gsub('(.*, )|(\\..*)','',full$Name) #chu truoc dau , va sau dau . duoc replace thanh khoang trong
table(full$Sex,full$Title) #dem gioi tinh theo title

#doi ten cac chuc danh co so luong khong dang ke
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

#sua lai cac ten cho dung
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'

full$Surname <- sapply(full$Name,function(x) strsplit(x, split = '[,.]')[[1]][1])

# Show title counts by sex again
table(full$Sex, full$Title)

count=nlevels(factor(full$Surname))
count

full$Fsize <- full$SibSp + full$Parch + 1
full$Family <- paste(full$Surname, full$Fsize, sep='_')

full$Deck <- substr(full$Cabin,1,1)
countd<- nlevels(factor(full$Deck))
countd 
# Replace missing fare value with median fare for class/embarkment
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)


fulling <- full[,c("Survived","Pclass","Sex","Age","SibSp","Parch","Fare","Embarked","Title","Deck","Fsize","Family")]
list <- c("Survived","Pclass","Sex","Embarked","Title","Deck","Family")
fulling[list] <- lapply(fulling[list],function(x) as.factor(x))
str(fulling)



# Set a random seed
set.seed(6)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(fulling,m=5, method='rf')


# Save the complete output 
mice_output <- complete(mice_mod)
apply(apply(mice_output,2,is.na),2,sum)

ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()

full$FsizeD[full$Fsize == 1] <- 'single'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'

mosaicplot(table(full$FsizeD, full$Survived), main='Family Size by Survival', shade=TRUE)

ggplot(mice_output,aes(x = Age, fill = factor(Survived))) + geom_histogram()+ facet_grid(.~Sex)

table(mice_output$Sex,mice_output$Survived)

mice_output$Child[mice_output$Age < 18] <- 'Child'
mice_output$Child[mice_output$Age>= 18] <- 'Adult'
table(mice_output$Child,mice_output$Survived)
ggplot(mice_output,aes(x = Age, fill = factor(Survived))) + geom_bar(stat = "count")+ facet_grid(.~Child)

mice_output$Mother <- 'Not Mother'
mice_output$Mother[mice_output$Sex == 'female'& mice_output$Age > 18 & mice_output$Parch > 0 & mice_output$Title != 'Miss'] <- 'Mother'
table(mice_output$Mother,mice_output$Survived)

list1 <- c("Child","Mother")
mice_output[list1] <- lapply(mice_output[list1],function(x) as.factor(x))
variables <- c("Pclass","Sex","Age","SibSp","Parch","Fare","Embarked","Title","Deck","Fsize","Child","Mother","Survived")
mice_output <- mice_output[,variables]
str(mice_output)
tapply(mice_output$Fare,mice_output$Pclass,mean)
tapply(mice_output$Fare,mice_output$Deck,mean)

# H0 : There is no difference between the two population means.
# H1 : There is difference between the two population means.

t.test(Fare~Survived,data = mice_output)

# H0 : There is no difference between the two population means.
# H1 : There is difference between the two population means.

t.test(Age~Survived,data = mice_output)

# H0 : Passenger Class and Survivals are independent.
# H1 : Passenger Class and Survivals are not independent.

chisq.test(mice_output$Pclass,mice_output$Survived)

training <- mice_output[c(1:891),]
testing <- mice_output[c(892:1309),]

model1 <- glm(Survived ~ ., family = binomial(link = 'logit'), data = training)
anova(model1, test = 'Chisq')
pR2(model1)

fit1 <- predict.lm(model1,newdata = testing,type = 'response')
fit1 <- ifelse(fit1 > 0.5,1,0)
misclassificationerror <- mean( fit1 != testing$Survived)
print(paste('Accuracy of Logistic Regression Model = ', 1-misclassificationerror))

model3 <- rpart(Survived ~ ., data = training,)

printcp(model3)


fit3 <- predict(model3,newdata = testing,type="prob")[,2]
fit31 <- predict(model3,testing, type="class")
t <- table(fit31,testing$Survived)
f<-sum(diag(t))/sum(t)
print(paste('Accuracy of Classification tree Model = ', f))

model2 <- randomForest(Survived ~ ., data = training)
model2
varImpPlot(model2)
fit2 <- predict(model2,newdata = testing)
confusionMatrix(fit2, testing$Survived)
a <- confusionMatrix(fit2, testing$Survived)$overall[1]
print(paste('Accuracy of Random Forest Model = ', a))

model4 <- svm(Survived ~ ., data = training)
model4

fit4 <- predict(model4, newdata = testing)
confusionMatrix(fit4, testing$Survived)

d <- confusionMatrix(fit4, testing$Survived)$overall[1]
print(paste('Accuracy of Support Vector Machine Model = ', d))

model5 <- train(Survived ~ .,data = training, method = "lda")
model5

fit5 <- predict(model5, newdata = testing)
confusionMatrix(fit5, testing$Survived)

l <- confusionMatrix(fit5, testing$Survived)$overall[1]
print(paste('Accuracy of Linear Discriminent Accuracy Model = ', l))


ensemble <- 0.2*(as.numeric(fit1)-1) + 0.4*(as.numeric(fit2)-1) + 0.4*(as.numeric(fit31)-1)
ensemble <- sapply(ensemble, round)
confusionMatrix(as.factor(ensemble),as.factor(testing$Survived))

g <- confusionMatrix(as.factor(ensemble), testing$Survived)$overall[1]
print(paste('Accuracy of Ensembled Model = ', g))







importance    <- importance(model2)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

prediction <-  predict(model2,testing, type="class")
submit <- data.frame(PassengerId = testing$PassengerId, Survived = prediction)
write.csv(submit, file = "C:/Users/uongt/Downloads/titanic/LRpredict.csv", row.names = FALSE)



