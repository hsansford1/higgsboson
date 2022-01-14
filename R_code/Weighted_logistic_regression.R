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
install_github("https://github.com/hsansford1/higgsboson")
library(higgsboson)
library(mlr)

######################################################
source('./R/useful_functions.R')
# Useful variables

train <- higgsboson::training

df_train <- train[,2:33] #remove eventid
df_train <- df_train[,-31] #remove weights

df_train$Label <- ifelse(df_train$Label=="s",1,0) #encode "s" and "b" to 1 - 0 (resp.) for logistic regresion
df_train$Label <- as.factor(df_train$Label) #need this as factor for caret

weights <- reweight(train$Weight, df_train$Label, Ns(), Nb()) # extract weights

# add a test in for weights?
all.equal(sum(weights[df_train$Label == 1]), Ns())
all.equal(sum(weights[df_train$Label == 0]), Nb())

######################################################################

#Missing values (and standardisation)

df_train[df_train==-999] <- 0

#if we want to standardise:

#st_train <- df_train
#st_train <- as.data.frame(scale(df_train[,1:30])) #standardisation
#st_train["Label"] <- label_factor



######################################################################

# Logistic regression with custom metric AMS with mlr package
#idea of mlr package is similar to scikit learn: create a Task -> make a learner -> train.
# We want to asnwer to: What is the AMS we can get on "the best" test set?


#install_github("https://github.com/mlr-org/mlr", force = T)
#library(mlr)

# Package mlr allows to use custom metrics (AMS here)

#define task
st_train <- as.data.frame(scale(df_train[,1:30]))
st_train$Label <- as.factor(df_train$Label)


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
AMS_mlr <- AMS_measure()
cv.logistic <- crossval(learner = logistic.learner, task = trainTask, iters = 5,
                        stratify = TRUE,
                        measures = AMS_mlr,
                        show.info = F, models=TRUE)
cv.logistic$aggr      
cv.logistic$measures.test # able to get AMS = no more than 0.3

#fmodel <- cv.logistic$models[[2]]

#get the trained model
fmodel <- mlr::train(logistic.learner,trainTask)
getLearnerModel(fmodel)



# get s and b using weights
pred <- predict(fmodel, trainTask)
truth <- pred$data$truth
response <- pred$data$response

# Replace with call to AMS_weighted instead? XXX
s <- sum(weights[(truth == 1) & (response == 1)])
b <- sum(weights[(truth == 0) & (response == 1)])

AMS_base(s,b) 
# XXX

#####################################################################

# Two-stage maximisation of AMS: Kotlowsky's paper

#Idea: learn a model (e.g. log reg) f
#calibrate classification threshold theta by maximising \hat(AMS)(theta) on a validation set

#create the validation set (caret). This should preserve the overall class distribution of the data
st_train <- as.data.frame(scale(df_train[,1:30]))
st_train$Label <- as.factor(df_train$Label)


trainIndex <- createDataPartition(st_train$Label, p = .8, list = FALSE, times = 1)
head(trainIndex)

Train <- st_train[ trainIndex,]
Valid  <- st_train[-trainIndex,]


#Logistic regression on Train: CV and sensitivity as metric

weights_Train <- reweight(weights[trainIndex], Train$Label, Ns(), Nb())
weights_Valid <- reweight(weights[-trainIndex], Valid$Label, Ns(), Nb())


#train_control <- trainControl(method = "cv", number = 10)

logreg_weighted2 <- caret::train(Label ~ .,
                       data = Train,
                       method = "glm",
                       metric="sensitivity",
                       weights = weights_Train,
                       family=binomial()
)


print(logreg_weighted2)
cm <- confusionMatrix(logreg_weighted2)
TPR <- cm$table[2,2]/(cm$table[2,2]+cm$table[1,2]) #TPR - sensitivity
FPR <- cm$table[2,1]/(cm$table[2,1]+cm$table[1,1])
TPR

# sensitivity is very low: change threshold. How? Maximising AMS on Valid

#Plot AMS for small values of threshold theta

theta_vals <- as.data.frame(seq(0.0001, 0.5, length.out=500)) # generate small sample thresholds theta
AMS_vals <- apply(theta_vals, 1, AMS(logreg_weighted2,Valid[,1:30],Valid[31], weights_Valid)) #compute AMS(theta)
plot(as.array(unlist(theta_vals)), AMS_vals, xlab="theta", ylab="AMS(theta)", pch=19) #plot it

max_theta <- theta_vals[which.max(AMS_vals),1]
max_AMS <- AMS_vals[which.max(AMS_vals)]
max_AMS



#####################################################################################

# Cross-Validation function for choosing threshold

threshold_CV <- function(df, label, weights, theta_0, theta_1, k=5, n=200){

  theta_vals <- as.data.frame(seq(theta_0, theta_1, length.out=n))
  max_thetas <- rep(0,k)
  AMS_vals <- matrix(0, nrow=n, ncol=k)

  for (i in 1:k){

    trainIndex <- createDataPartition(df_train$Label, p = .8, list = FALSE, times = 1)

    Train <- df[ trainIndex,]
    Train$Label <- label[trainIndex]
    Valid  <- df[-trainIndex,]
    Valid$Label <- label[-trainIndex]

    weights_Train <- reweight(weights[trainIndex], Train$Label, Ns(), Nb())
    weights_Valid <- reweight(weights[-trainIndex], Valid$Label, Ns(), Nb())

    logreg_weighted <- caret::train(Label ~ .,
                                  data = Train,
                                  method = "glm",
                                  metric="sensitivity",
                                  weights = weights_Train,
                                  family=binomial()
   )

   AMS_vals[,i] <- apply(theta_vals, 1, AMS(logreg_weighted,Valid[,1:30],Valid[31], weights_Valid))
   max_thetas[i] <- theta_vals[which.max(AMS_vals[,i]),1]
  }
  AMS_mean <- apply(AMS_vals, 1, mean)
  AMS_sd <- apply(AMS_vals, 1, sd)
  plot(as.array(unlist(theta_vals)), AMS_mean, xlab="theta", ylab="AMS(theta)", pch=19)
  lines(as.array(unlist(theta_vals)), AMS_mean + mean(AMS_sd), col='red')
  lines(as.array(unlist(theta_vals)), AMS_mean - mean(AMS_sd), col='red')
  max_theta <- theta_vals[which.max(AMS_mean),1]
  max_AMS <- AMS_mean[which.max(AMS_mean)]
  return(list('max_theta'=max_theta, 'max_AMS'=max_AMS, 'AMS_sd'=AMS_sd, 'max_thetas'=max_thetas))
}

#df_train[df_train==-999] <- 0
#st_train <- as.data.frame(scale(df_train[,1:30]))

theta_CV <- threshold_CV(st_train, df_train$Label, weights, theta_0=0.0001, theta_1=0.1)
theta_CV



#######################################################################

#Threshold tuning after PCA-dimensional reduction

st_train_pca <- as.data.frame(scale(df_train[1:30])) # standardise the variables
train.pca <- prcomp(st_train_pca)
summary(train.pca)
screeplot(train.pca, type="lines")

#the first pca has by far the highest variance
#Let's try to keep the first 3 pca's (they have variance bigger than 2)

st_train_dimred <- data.frame("PCA1"=train.pca$x[,1],"PCA2"=train.pca$x[,2],"PCA3"=train.pca$x[,3])
st_train_dimred["Label"] <- as.factor(df_train$Label)


trainIndex <- createDataPartition(st_train_dimred$Label, p = .8,
                                  list = FALSE,
                                  times = 1)
head(trainIndex)

Train <- st_train_dimred[ trainIndex,]
Valid  <- st_train_dimred[-trainIndex,]


#First stage: Logistic regression on Train: CV and sensitivity as metric

weights_Train <- reweight(weights[trainIndex], Train$Label, Ns(), Nb())
weights_Valid <- reweight(weights[-trainIndex], Valid$Label, Ns(), Nb())


#train_control <- trainControl(method = "cv", number = 10)

logreg_weighted_pca <- caret::train(Label ~ .,
                                    data = Train,
                                    method = "glm",
                                    #metric="sensitivity",
                                    weights = weights_Train,
                                    family=binomial()
)




print(logreg_weighted_pca)
cm <- confusionMatrix(logreg_weighted_pca)
TPR <- cm$table[2,2]/(cm$table[2,2]+cm$table[1,2]) #TPR - sensitivity
FPR <- cm$table[2,1]/(cm$table[2,1]+cm$table[1,1])
TPR


#Plot AMS for small values of threshold theta

theta_vals <- as.data.frame(seq(0.0001, 0.05, length.out=500)) # generate small sample thresholds theta
AMS_vals <- apply(theta_vals, 1, AMS(logreg_weighted_pca,Valid[,1:3],Valid[4], weights_Valid)) #compute AMS(theta)
plot(as.array(unlist(theta_vals)), AMS_vals, xlab="theta", ylab="AMS(theta)", pch=19) #plot it

max_theta <- theta_vals[which.max(AMS_vals),1]
max_theta
max_AMS <- AMS_vals[which.max(AMS_vals)]
max_AMS

# Choose the threshold

theta_CV <- threshold_CV(st_train_dimred[,1:3], st_train_dimred$Label, weights=weights, theta_0=0.0001, theta_1=0.01, k=5, n=200)
theta_CV

