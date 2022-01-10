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
library(mlr)

######################################################

# Useful variables

train <- higgsboson::training

df_train <- train[,2:33] #remove eventid
df_train <- df_train[,-31] #remove weights

df_train$Label=ifelse(df_train$Label=="s",1,0) #encode "s" and "b" to 1 - 0 (resp.) for logistic regresion

label_factor=as.factor(df_train$Label)
df_train["Label"]=label_factor #need this as factor for caret

weights <- train$Weight


#-----------------------------------------------------------------------------

#Treat MISSING VALUES: set all the -999 to zero and normalise features

#Question: do we have to recompute weights if we also standardise?

df_train[df_train==-999] <- 0

st_train <- df_train # if we do not want to standardise too.

#if we want to standardise:

#st_train <- as.data.frame(scale(df_train[,1:30])) #standardisation
#st_train["Label"] <- label_factor


#-----------------------------------------------------------------------

# Logistic regression with standardised data - AMS as direct measure


#define task
trainTask <- makeClassifTask(data = st_train,
                             target = "Label",
                             positive = 1,
                             weights = weights
)
trainTask

#make learner
logistic.learner <- makeLearner("classif.logreg",
                                predict.type = "response")

#cv training
cv.logistic <- crossval(learner = logistic.learner, task = trainTask, iters = 5 ,
                        stratify = FALSE,
                        measures = AMS_mlr,
                        show.info = F)
cv.logistic$aggr
cv.logistic$measures.test

#get the trained model
fmodel <- train(logistic.learner,trainTask)
getLearnerModel(fmodel)





#-----------------------------------------------------------------------

# Logistic regression with standardised data - AMS for threshold tuning with CV

trainIndex <- createDataPartition(st_train$Label, p = .8,
                                  list = FALSE,
                                  times = 1)
head(trainIndex)

Train <- st_train[ trainIndex,]
Valid  <- st_train[-trainIndex,]


#First stage: Logistic regression on Train: CV and sensitivity as metric

N_s <- sum(weights[st_train$Label == 1])
N_b <- sum(weights[st_train$Label == 0])

weights_Train <- weights[trainIndex]
weights_Valid <- weights[-trainIndex]



weights_Train <- reweight(weights_Train, Train$Label, N_s, N_b)
weights_Valid <- reweight(weights_Valid, Valid$Label, N_s, N_b)

train_control <- trainControl(method = "cv", number = 10)

logreg_weighted <- caret::train(Label ~ .,
                                data = Train,
                                trControl = train_control,
                                method = "glm",
                                metric="sensitivity",
                                weights = weights_Train,
                                family=binomial()
)


#Second stage: 

#Plot AMS for small values of threshold theta and check where it is max

theta_vals <- as.data.frame(seq(0.0001, 0.05, length.out=500)) # generate small sample thresholds theta
AMS_vals <- apply(theta_vals, 1, AMS(logreg_weighted,Valid[,1:30],Valid[31], weights_Valid)) #compute AMS(theta)
plot(as.array(unlist(theta_vals)), AMS_vals, xlab="theta", ylab="AMS(theta)", pch=19) #plot it





# tune theta with CV
theta_CV <- threshold_CV(st_train[,1:30], st_train$Label, weights, theta_0=0.002, theta_1=0.08, k=5)
theta_CV


















#------------------------------------------------------------------------

# SVM after PCA?




























