library(tidyverse)
library(reshape2)
library(ggplot2)
library(glmnet)
library(ROCR)
library(caret)
library(cvms)
library(tibble) 
library(pROC)
library(checkmate)
library(BBmisc)
library(testit)

library(devtools)
#install_github("https://github.com/hsansford1/higgsboson")
library(higgsboson)

######################################################

# Useful variables

train <- higgsboson::training

df_train <- train[,2:33] #remove eventid
df_train <- df_train[,-31] #remove weights


df_train$Label=ifelse(df_train$Label=="s",1,0) #encode "s" and "b" to 1 - 0 (resp.) for logistic regresion
label_factor=as.factor(df_train$Label)
df_train["Label"]=label_factor #need this as factor for caret

weights <- train$Weight # extract weights



######################################################################


#Weighted Logistic regression with common metrics (e.g. sensitivity)


train_control <- trainControl(method = "cv", number = 10) 

# !!! future task is to modify sensitivity to AMS metric !!!
model_weights <- train(Label ~ .,
                       data = df_train,
                       trControl = train_control,
                       method = "glm",
                       metric="sensitivity",
                       weights = weights,
                       family=binomial()
)



print(model_weights)
cm <- confusionMatrix(model_weights)
TPR <- cm$table[2,2]/(cm$table[2,2]+cm$table[1,2]) #TPR - sensitivity
FPR <- cm$table[2,1]/(cm$table[2,1]+cm$table[1,1])

# print confusion matrix, sens. and spec.
#cm<- confusionMatrix(model_weights)
plt <- as.data.frame(round(cm$table,1))
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))

ggplot(plt, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Prediction",y = "Reference") 



########################################################################

# Logistic regression with custom metric AMS with mlr package
#idea of mlr package is similar to scikit learn: create a Task -> make a learner -> train.


#install_github("https://github.com/mlr-org/mlr", force = T)
library(mlr)



# Create measure AMS using makeMeasure() from mlr

AMS_hb = function(truth, response) {
  
  conf.mat = table(response, truth)
  conf.mat = conf.mat / sum(conf.mat)
  s  <- conf.mat[2,2]/(conf.mat[2,2]+conf.mat[1,2]) #TPR - sensitivity
  b  <- conf.mat[2,1]/(conf.mat[2,1]+conf.mat[1,1])
  
  sqrt(2*((s+b+10)*log(1+s/(b+10))-s)) #calculate AMS
  
}



AMS = makeMeasure(
  id = "AMS_hb", minimize = FALSE, best = 1, worst = -1,
  properties = c("classif"),
  name = "Approximate median significance",
  fun = function(task, model, pred, feats, extra.args) {
    AMS_hb(pred$data$truth, pred$data$response)
  }
)



# Package mlr allows to use custom metrics (AMS here)

#define task
trainTask <- makeClassifTask(data = df_train,target = "Label", positive = 1)
trainTask 

#make learner
logistic.learner <- makeLearner("classif.logreg",predict.type = "response")

#cv training
cv.logistic <- crossval(learner = logistic.learner,task = trainTask, iters = 10,stratify = TRUE,measures = AMS, show.info = F)
cv.logistic$aggr
cv.logistic$measures.test

#get the trained model
fmodel <- train(logistic.learner,trainTask)
getLearnerModel(fmodel)

