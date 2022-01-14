library(tidyverse)
# library(reshape2)
# library(ggplot2)
# library(glmnet)
# library(ROCR)
library(caret)
# library(cvms)
# library(tibble)
# library(pROC)
# library(checkmate)
# library(BBmisc)
# library(testit)
# library(devtools)

library(higgsboson)

# ---- load training data ----

train <- higgsboson::training

df_train <- train[,2:33] #remove eventid
df_train <- df_train[,-31] #remove weights

df_train$Label <- ifelse(df_train$Label=="s",1,0) #encode "s" and "b" to 1 - 0 (resp.) for logistic regresion
df_train$Label <- as.factor(df_train$Label) #need this as factor for caret

weights <- reweight(train$Weight, df_train$Label, Ns(), Nb()) # extract weights

# add a test in for weights?
all.equal(sum(weights[df_train$Label == 1]), Ns())
all.equal(sum(weights[df_train$Label == 0]), Nb())

# feature selection -------------------------------------------------------

# # as in kaggle winning entry, try removing azimuthal angle features
# df_train <- df_train %>% select(!ends_with('phi'))

# partition training/validation data --------------------------------------

# set aside 20% of training data for validation
trainIndex <- createDataPartition(df_train$Label, p = .8,list = FALSE,times = 1)
Train <- df_train[ trainIndex,]
Valid  <- df_train[-trainIndex,]

# reweight training and validation sets
weights_Train <- weights[trainIndex]
weights_Valid <- weights[-trainIndex]

weights_Train <- reweight(weights_Train, Train$Label, Ns(), Nb())
weights_Valid <- reweight(weights_Valid, Valid$Label, Ns(), Nb())

# missing values ----------------------------------------------------------

#Missing values (and standardisation)

Train[Train==-999] <- 0
Valid[Valid==-999] <- 0

#if we want to standardise:

Train_st <- Train %>%
  mutate(across(!Label, scale))
Valid_st <- Valid %>%
  mutate(across(!Label, scale))

# Try and find a model! ---------------------------------------------------

# # for speed while we're tweaking things
# nrows <- 50000
# Train_st <- slice_head(Train_st, n=nrows)
# weights_Train <- weights_Train[1:nrows]
# weights_Train <- reweight(weights_Train, Train_st$Label, Ns(), Nb())

# NB: with regularisation, might make sense to include interaction terms
#     e.g. Label ~ .^2 (all pairwise interactions), since regularising should
#     stop overfitting (especially with alpha=1, where many coefficients -> 0)

# Strategy 1: train without changing threshold ----------------------------
#             model trained without AMS,
#             then threshold chosen to maximise AMS on validation set

# glmnet seems to need Label to be factor w/ levels 's', 'b' (not 0, 1)
levels(Train_st$Label)[levels(Train_st$Label)=="0"] <- "b"
levels(Train_st$Label)[levels(Train_st$Label)=="1"] <- "s"

# try more hyperparameters values!!!
paramGrid <-  expand.grid(alpha = c(0,0.1,0.25,0.5,0.75,1),
                          lambda = c(0,0.00001,0.0001,0.001,0.01,0.1,1))

train_control1 <- trainControl(method = 'cv',
                              number = 5,
                              classProbs = TRUE,
                              savePredictions = TRUE,
                              returnData = TRUE,
                              summaryFunction = twoClassSummary)

caret_glmnet1 <- caret::train(Label ~ .,
                              Train_st,
                              trControl = train_control1,
                              metric = 'Spec',
                              method = "glmnet",
                              tuneGrid = paramGrid,
                              weights = weights_Train
)

# Almost no 's' are predicted during cv
# (if ~30000 actual 's' observed, only 5-10 's' predicted)
# playing with summaryFunction/metric doesn't seem to help

# Find theta based on Valid set

levels(Valid_st$Label)[levels(Valid_st$Label)=="0"] <- "b"
levels(Valid_st$Label)[levels(Valid_st$Label)=="1"] <- "s"

probs <- predict(caret_glmnet1, newdata = Valid_st, type='prob')
theta_seq <- seq(0.00001, 0.05, length.out=100)
ams_vals <- vector(mode='numeric', length=0)
for(theta in theta_seq){
  preds <- if_else(probs$s > theta, 's', 'b')
  ams_vals <- append(ams_vals, AMS_weighted_gen(Valid_st$Label, preds, weights_Valid))
}

plot(theta_seq, ams_vals)
theta_max <- theta_seq[ams_vals==max(ams_vals)]

# Try this theta on test data:
# preprocess test data
test <- higgsboson::test

df_test <- test[,2:33] #remove eventid
df_test <- df_test[,-31] #remove weights
df_test$Label <- ifelse(df_test$Label=="s",1,0) #encode "s" and "b" to 1 - 0 (resp.) for logistic regresion
df_test$Label <- as.factor(df_test$Label) #need this as factor for caret
weights_test <- reweight(test$Weight, df_test$Label, Ns(), Nb()) # extract weights
all.equal(sum(weights_test[df_test$Label == 1]), Ns())
all.equal(sum(weights_test[df_test$Label == 0]), Nb())
df_test[df_test==-999] <- 0
Test <- df_test %>%
  mutate(across(!Label, scale))

probs_test <- predict(caret_glmnet1, newdata=Test, type='prob')
preds_test <- probs_test$s > theta_max

AMS_weighted(df_test$Label, preds_test, weights_test)
ams_test_vals <- vector(mode='numeric', length=0)
theta_seq_test <- seq(0.00001, 0.1, length.out=100)
for(theta in theta_seq_test){
  preds_test_loop <- if_else(probs_test$s > theta, 1, 0)
  ams_test_vals <- append(ams_test_vals, AMS_weighted_gen(df_test$Label, preds_test_loop, weights_test))
}

plot(theta_seq_test, ams_test_vals) # promising!!!
theta_max <- theta_seq_test[ams_test_vals==max(ams_test_vals)]

# Strategy 1.5: Train as in strategy 1 for alpha, lambda, but k times, with k-fold CV for theta ----

threshold_CV_enet <- function(df, label, weights, theta_0, theta_1, k=5, n=50, alpha=0, lambda=0){

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

    Train$Label <- as.factor(ifelse(Train$Label == 1, 's', 'b'))

    enet_logreg_weighted <- caret::train(Label ~ .,
                                         trControl = trainControl(method='none',
                                                                  classProbs = TRUE,
                                                                  savePredictions = TRUE,
                                                                  returnData = TRUE,
                                                                  summaryFunction = twoClassSummary),
                                         data = Train,
                                         method = "glmnet",
                                         metric = "Sens",
                                         weights = weights_Train,
                                         tuneGrid = expand.grid(alpha = 0,
                                                                lambda = 0)
    )

    probs <- predict(enet_logreg_weighted, newdata = Valid, type='prob')$s
    ams_column <- vector(mode='numeric', length=0)
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

  AMS_vals <- apply(AMS_vals, 1, mean)
  plot(as.array(unlist(theta_vals)), AMS_vals, xlab="theta", ylab="AMS(theta)", pch=19)
  max_theta <- theta_vals[which.max(AMS_vals)]
  max_AMS <- AMS_vals[which.max(AMS_vals)]
  return(c(max_theta=max_theta, max_AMS=max_AMS))
  return(enet_logreg_weighted)
}
temp <- threshold_CV_enet(st_train, st_train$Label, weights_Train, theta_0=0, theta_1=1, k=2)
# Strategy 2: train with AMS ---------------------------------------------------

# can implement manual loss function (summaryFunction argument to trainControl)
# so can use AMS directly as cv training loss

# make loss function ams for caret trainControl
ams_summaryFn <- function(data, lev=NULL, model=NULL){
  ams <- AMS_weighted_gen(data$obs, data$pred, data$weights)
  names(ams) <- 'AMS'
  return(ams)
}

train_control2 <- trainControl(method = 'cv',
                              number = 5,
                              classProbs = TRUE,
                              savePredictions = TRUE,
                              returnData = TRUE,
                              summaryFunction = ams_summaryFn)

caret_glmnet2 <- caret::train(Label ~ .,
                              Train_st,
                              trControl = train_control2,
                              maximise = TRUE,
                              method = "glmnet",
                              metric = 'AMS',
                              weights = weights_Train
)

# Fails on such unbalanced data!

# # Works fine if we pick 's' and 'b' with 50/50 probability instead:
# Train$Label <- factor(sample(c('s','b'), 10000, replace=TRUE), levels=c('s','b'))
# caret::train(Label ~ .,
#              Train,
#              trControl = train_control2,
#              maximise = TRUE,
#              method = "glmnet",
#              metric = 'AMS',
#              weights = weights_Train


# Strategy 2.5: Train with AMS, fudged ------------------------------------

# Try altering the loss function so it calculates a kind of proxy AMS
# based on class probability (same idea as altering threshold theta)

ams_summaryFn_classProbCutoff <- function(data, lev=NULL, model=NULL){
  cutoff <- 0.001
  hackyPreds <- if_else(data$s > cutoff, 's', 'b') %>% as.factor()
  print(sum(hackyPreds=='s'))
  ams <- ams_fromWeights(hackyPreds, data$obs, data$weights)
  names(ams) <- 'AMS'
  return(ams)
}

train_control2.5 <- trainControl(method = 'cv',
                               number = 3,
                               classProbs = TRUE,
                               savePredictions = TRUE,
                               returnData = TRUE,
                               summaryFunction = ams_summaryFn_classProbCutoff)

caret_glmnet2.5 <- caret::train(Label ~ .,
                              Train,
                              trControl = train_control2.5,
                              maximise = TRUE,
                              method = "glmnet",
                              metric = 'AMS',
                              weights = weights_Train)


# Strategy 3: Train using AMS & vary threshold in process -----------------

# I'll have a go at implementing this for our case:
# https://topepo.github.io/caret/using-your-own-model-in-train.html#illustrative-example-5-optimizing-probability-thresholds-for-class-imbalances

# Play with new AMS functions

load('./data/test.RData')
test$reweighted <- reweight(test$Weight, test$Label, Ns(), Nb()) # reweight using new function

probs <- predict(caret_glmnet1, newdata=Valid, type='prob')
test$predictions <- if_else(probs$s > 0.001, 's', 'b')
AMS_weighted_gen(test$Label, test$predictions, test$Weight) # no reweighted, AMS=0.9
AMS_weighted_gen(test$Label, test$predictions, test$reweighted) # reweighted, AMS=1.1

AMS_weighted_gen()
