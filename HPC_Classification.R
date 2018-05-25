set.seed(2017)
library(naivebayes)
trainingfit <- sample(nrow(schedulingData),nrow(schedulingData)*0.6,replace = F)
training <- schedulingData[trainingfit,]
testing <- schedulingData[-trainingfit,]
nb <- naive_bayes(formula = Class ~ ., data = training)
nb.predict <- predict(nb,newdata=testing)
confusionMatrix(nb.predict,testing$Class)
table(nb.predict, testing$Class)

### Boosting ###### 
set.seed(2017)
library(adabag) # Loading Adabag package
sd.boost <- boosting(Class ~ .,data = training)
summary(sd.boost)

# Boosting prediction on testing data 
pr.boost <- predict.boosting(sd.boost,newdata = testing)

#cross validation of the Boosting model
sd.boost.cv <- boosting.cv(Class ~ ., schedulingData, v = 5)
sd.boost.cv

#variable importance Plot
importanceplot(sd.boost)



#Decision tree 
set.seed(2017)
library(rpart) # loading rpart package 

#training a decision tree on training data
sd.tree <- rpart(Class ~., data = training)
plot(sd.tree, pretty = 1) # plotting the tree
text(sd.tree) # adding text to tree
sd.tree 

#Prediction using the decison tree on the testing data
pd.tree.sd <- predict(sd.tree, newdata = testing,type = "class")

#confusion matrix 
confusionMatrix(pd.tree.sd,testing$Class)


