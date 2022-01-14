# ElasticNet

library(tidyverse)
library(caret)
library(higgsboson)

# pre-processing ===============================================================
train <- higgsboson::training

df_train <- train[,2:33] #remove eventid
df_train <- df_train[,-31] #remove weights

df_train$Label <- ifelse(df_train$Label=="s",1,0) #encode "s" and "b" to 1 - 0 (resp.) for logistic regresion
df_train$Label <- as.factor(df_train$Label) #need this as factor for caret
weights <- reweight(train$Weight, df_train$Label, Ns(), Nb()) # extract weights

# missing values & standardise
Train <- df_train
Train[Train==-999] <- 0
Train_st <- Train %>% mutate(across(!Label, scale))

# Fit glmnet model using CV with AMS as metric =================================

# implement manual loss function (summaryFunction argument to trainControl)
# so can use AMS directly as CV training loss

# make loss function ams for caret trainControl
AMS_summaryFn <- function(data, lev=NULL, model=NULL){
  AMS <- AMS_weighted(data$obs=='s',
                      data$pred=='s',
                      reweight(data$weights, data$obs, Ns(), Nb()))
  names(AMS) <- 'AMS'
  return(AMS)
}

train_control_AMS <- trainControl(method = 'cv',
                               number = 3,
                               classProbs = TRUE,
                               savePredictions = TRUE,
                               returnData = TRUE,
                               summaryFunction = AMS_summaryFn)

# glmnet seems to need Label to be factor w/ levels 's', 'b' (not 0, 1)
levels(Train_st$Label)[levels(Train_st$Label)=="0"] <- "b"
levels(Train_st$Label)[levels(Train_st$Label)=="1"] <- "s"

# try hyperparameter values
paramGrid <-  expand.grid(alpha = c(0,0.1,0.25,0.5,0.75,1),
                          lambda = c(0,0.00001,0.0001,0.001,0.01,0.1,1))

caret_glmnet_AMS <- caret::train(Label ~ .,
                              Train_st,
                              trControl = train_control_AMS,
                              maximise = TRUE,
                              method = "glmnet",
                              metric = 'AMS',
                              tuneGrid = paramGrid,
                              weights = weights)

# 2-stage maximisation with threshold_CV =======================================
# as with 'glm':
# - within each of k folds of CV, train model (without reference to AMS)
# - use CV to pick theta maximising average AMS (averaged over k folds)
# but with glmnet, need to choose hyperparameters first:

# glmnet seems to need Label to be factor w/ levels 's', 'b' (not 0, 1)
levels(Train_st$Label)[levels(Train_st$Label)=="0"] <- "b"
levels(Train_st$Label)[levels(Train_st$Label)=="1"] <- "s"

# try hyperparameter values
paramGrid <-  expand.grid(alpha = c(0,0.1,0.25,0.5,0.75,1),
                          lambda = c(0,0.00001,0.0001,0.001,0.01,0.1,1))

caret_glmnet_2stg <- caret::train(Label ~ .,
                                  Train_st,
                                  trControl = trainControl(method = 'cv', number = 10),
                                  metric = 'Kappa',
                                  method = "glmnet",
                                  tuneGrid = paramGrid,
                                  weights = weights)

# If we had interesting hyperparameters (i.e. lamba != 0)
# would then use this version of threshold_CV
# (slightly tweaked as glmnet models have slightly different structure to glm)
threshold_CV_glmnet <- function(df, label, weights, theta_0, theta_1, k=5, n=50, ...){

  theta_vals <- seq(theta_0, theta_1, length.out=n)
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

    glmnet_logreg_weighted <- caret::train(Label ~ .,
                                           trControl = trainControl(method='none',
                                                                    #classProbs = TRUE,
                                                                    #savePredictions = TRUE,
                                                                    returnData = TRUE),
                                           data = Train,
                                           method = "glmnet",
                                           metric = "Kappa",
                                           weights = weights_Train,
                                           tuneGrid = expand.grid(...))

    probs <- predict(glmnet_logreg_weighted, newdata = Valid, type='prob')$s
    ams_column <- vector(mode='numeric', length=0)
    Valid$Label <- Valid$Label == 's'
    for(theta in theta_vals){
      preds <- probs > theta
      ams_column <- append(ams_column, AMS_weighted(Valid$Label, preds, weights_Valid))
    }
    AMS_vals[,i] <- ams_column
    max_thetas[i] <- theta_vals[which.max(AMS_vals[,i])]
  }

  AMS_mean <- apply(AMS_vals, 1, mean)
  AMS_sd <- apply(AMS_vals, 1, sd)
  plot(as.array(unlist(theta_vals)), AMS_mean, xlab="theta", ylab="AMS(theta)", pch=19)
  lines(as.array(unlist(theta_vals)), AMS_mean + mean(AMS_sd), col='red')
  lines(as.array(unlist(theta_vals)), AMS_mean - mean(AMS_sd), col='red')
  max_theta <- theta_vals[which.max(AMS_mean)]
  max_AMS <- AMS_mean[which.max(AMS_mean)]

  return(list('max_theta'=max_theta, 'max_AMS'=max_AMS, 'AMS_sd'=AMS_sd, 'max_thetas'=max_thetas))
}

threshold_CV_glmnet(Train_st, Train_st$Label, weights,
                    theta_0 = 0, theta_1 = 0.1,
                    k = 2,
                    alpha=caret_glmnet_2stg$bestTune$alpha, lambda=caret_glmnet_2stg$bestTune$lambda)

