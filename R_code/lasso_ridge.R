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

# missing values ----------------------------------------------------------

#Missing values (and standardisation)

df_train[df_train==-999] <- 0

#if we want to standardise:

#st_train <- df_train
#st_train <- as.data.frame(scale(df_train[,1:30])) #standardisation
#st_train["Label"] <- label_factor


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

# Try and find a model! ---------------------------------------------------

# # for speed while we're tweaking things
nrows <- 50000
Train <- slice_head(Train, n=nrows)
weights_Train <- weights_Train[1:nrows]
weights_Train <- reweight(weights_Train, Train$Label, Ns(), Nb())

# NB: with regularisation, might make sense to include interaction terms
#     e.g. Label ~ .^2 (all pairwise interactions), since regularising should
#     stop overfitting (especially with alpha=1, where many coefficients -> 0)

# Strategy 1: train without changing threshold ----------------------------
#             model trained without AMS,
#             then threshold chosen to maximise AMS on validation set

# glmnet seems to need Label to be factor w/ levels 's', 'b' (not 0, 1)
levels(Train$Label)[levels(Train$Label)=="0"] <- "b"
levels(Train$Label)[levels(Train$Label)=="1"] <- "s"

# try more hyperparameters values!!! (Random search?)
train_control1 <- trainControl(method = 'cv',
                              number = 10,
                              classProbs = TRUE,
                              savePredictions = TRUE,
                              returnData = TRUE,
                              summaryFunction = twoClassSummary)

caret_glmnet1 <- caret::train(Label ~ .,
                              Train,
                              trControl = train_control1,
                              metric = 'Spec',
                              method = "glmnet",
                              weights = weights_Train
)

# Almost no 's' are predicted during cv
# (if ~30000 actual 's' observed, only 5-10 's' predicted)
# playing with summaryFunction/metric doesn't seem to help

# Find theta based on Valid set

levels(Valid$Label)[levels(Valid$Label)=="0"] <- "b"
levels(Valid$Label)[levels(Valid$Label)=="1"] <- "s"

probs <- predict(caret_glmnet1, newdata = Valid, type='prob')
theta_seq <- seq(0.00001, 0.1, length.out=10000)
ams_vals <- vector(mode='numeric', length=0)
for(theta in theta_seq){
  preds <- if_else(probs$s > theta, 's', 'b')
  ams_vals <- append(ams_vals, AMS_weighted_gen(Valid$Label, preds, weights_Valid))
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
df_train[df_train==-999] <- 0

probs_test <- predict(caret_glmnet1, newdata=df_test, type='prob')
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
# Strategy 1.5: Train as in strategy 1, but k times, with k-fold CV for theta

# TO DO TOMORROW!!

# Strategy 2: train with AMS ---------------------------------------------------

# can implement manual loss function (summaryFunction argument to trainControl)
# so can use AMS directly as cv training loss

# make loss function ams for caret trainControl
ams_summaryFn <- function(data, lev=NULL, model=NULL){
  ams <- ams_fromWeights(data$pred, data$obs, data$weights)
  names(ams) <- 'AMS'
  return(ams)
}

train_control2 <- trainControl(method = 'cv',
                              number = 3,
                              classProbs = TRUE,
                              savePredictions = TRUE,
                              returnData = TRUE,
                              summaryFunction = ams_summaryFn)

caret_glmnet2 <- caret::train(Label ~ .,
                              Train,
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

# BIGGER PROBLEM: AMS values no good!
#                 Values bigger than expected and change with sample size
#                 (running caret_glmnet2.5 with cutoff 0.001:
#                   10000 samples gives AMS ~20,
#                   50000 samples gives AMS ~50)
#
# Either function is wrong or weights are wrong
# (need to reweight for each cv sample maybe?)


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
