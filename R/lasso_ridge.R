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

# ---- load training data ----

load('./data/training.RData')
train <- training
rm(training)

df_train <- train[,2:33] #remove eventid
df_train <- df_train[,-31] #remove weights

#df_train$Label=ifelse(df_train$Label=="s",1,0) #encode "s" and "b" to 1 - 0 (resp.) for logistic regresion

label_factor=as.factor(df_train$Label)
df_train["Label"]=label_factor #need this as factor for caret

weights <- train$Weight # extract weights

# missing values ----------------------------------------------------------

# # 2. Set missing values (currently -999) to column mean
# df_train <- df_train %>%
#   mutate(across( !Label, ~na_if(.,-999) ))
# colmeans <- df_train %>%
#   summarise(across( !Label , function(x)mean(x, na.rm=TRUE) ))
# for(i in 1:30){
#   df_train[[i]] <- replace_na(df_train[[i]], colmeans[[i]])
# }

# feature selection -------------------------------------------------------

# # as in kaggle winning entry, try removing azimuthal angle features
# df_train <- df_train %>% select(!ends_with('phi'))


# partition training/validation data --------------------------------------

# set aside 20% of training data for validation
trainIndex <- createDataPartition(df_train$Label, p = .8,list = FALSE,times = 1)
Train <- df_train[ trainIndex,]
Valid  <- df_train[-trainIndex,]

# reweight training and validation sets
N_s <- sum(weights[df_train$Label == 1])
N_b <- sum(weights[df_train$Label == 0])
weights_Train <- weights[trainIndex]
weights_Valid <- weights[-trainIndex]

weights_Train <- reweight(weights_Train, Train$Label)
weights_Valid <- reweight(weights_Valid, Valid$Label)

# Try and find a model! ---------------------------------------------------

# for speed while we're tweaking things
nrows <- 50000
Train <- slice_head(Train, n=nrows)
weights_Train <- weights_Train[1:nrows]

# NB: with regularisation, might make sense to include interaction terms
#     e.g. Label ~ .^2 (all pairwise interactions), since regularising should
#     stop overfitting (especially with alpha=1, where many coefficients -> 0)

# Strategy 1: train without changing threshold ----------------------------
#             model trained without AMS,
#             then threshold chosen to maximise AMS on validation set

train_control1 <- trainControl(method = 'cv',
                              number = 3,
                              summaryFunction = twoClassSummary,
                              classProbs = TRUE,
                              savePredictions = TRUE,
                              returnData = TRUE)

caret_glmnet1 <- caret::train(Label ~ .,
                              Train,
                              trControl = train_control1,
                              metric = 'Spec',
                              maximise = TRUE,
                              method = "glmnet",
                              weights = weights_Train
)

sum(caret_glmnet1$pred$obs == 's')
sum(caret_glmnet1$pred$pred == 's')

# Almost no 's' are predicted during cv
# (if ~30000 actual 's' observed, only 5-10 's' predicted)
# playing with summaryFunction/metric doesn't seem to help

# Haven't tried tuning theta (decision rule threshold) with AMS Valid set yet
# Use caret::thresholder?

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
test$reweighted <- reweight2(test$Weight, test$Label) # reweight using new function

probs <- predict(caret_glmnet1, newdata=test, type='prob')
test$predictions <- if_else(probs$s > 0.001, 's', 'b')
AMS_weighted_gen(test$Label, test$predictions, test$Weight) # no reweighted, AMS=0.9
AMS_weighted_gen(test$Label, test$predictions, test$reweighted) # reweighted, AMS=1.1

AMS_weighted_gen()
