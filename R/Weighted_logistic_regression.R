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

######################################################
source('./R/useful_functions.R')
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

# Package mlr allows to use custom metrics (AMS here)

#define task
trainTask <- makeClassifTask(data = df_train,
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
cv.logistic$aggr   # If we do it with no weights AMS is larger...
cv.logistic$measures.test

#get the trained model
fmodel <- train(logistic.learner,trainTask)
getLearnerModel(fmodel)


# get s and b using weights
pred <- predict(fmodel, trainTask)
truth <- pred$data$truth
response <- pred$data$response

# Replace with call to AMS_weighted instead? XXX
s <- sum(weights[(truth == 1) & (response == 1)])
b <- sum(weights[(truth == 0) & (response == 1)])

AMS <- sqrt(2*((s+b+10)*log(1+s/(b+10))-s))
AMS
# XXX

#####################################################################

# Two-stage maximisation of AMS: Kotlowsky's paper

#Idea: learn a model (e.g. log reg) f
#calibrate classification threshold theta by maximising \hat(AMS)(theta) on a validation set

#create the validation set (caret). This should preserve the overall class distribution of the data

trainIndex <- createDataPartition(df_train$Label, p = .8,
                                  list = FALSE,
                                  times = 1)
head(trainIndex)

Train <- df_train[ trainIndex,]
Valid  <- df_train[-trainIndex,]


#Logistic regression on Train: CV and sensitivity as metric
N_s <- sum(weights[df_train$Label == 1])
N_b <- sum(weights[df_train$Label == 0])

weights_Train <- weights[trainIndex]
weights_Valid <- weights[-trainIndex]

# # Delete?? Already in useful_functions
# # function for reweighting
# reweight <- function(weights, labels, N_s, N_b){
#   new_weights <- weights
#   new_weights[labels == 1] <- weights[labels == 1] * N_s / sum(weights[labels == 1])
#   new_weights[labels == 0] <- weights[labels == 0] * N_b / sum(weights[labels == 0])
#   return(new_weights)
# }

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


print(logreg_weighted)
cm <- confusionMatrix(logreg_weighted)
TPR <- cm$table[2,2]/(cm$table[2,2]+cm$table[1,2]) #TPR - sensitivity
FPR <- cm$table[2,1]/(cm$table[2,1]+cm$table[1,1])
TPR


# sensitivity is very low: change threshold. How? Maximising AMS on Valid

#Function which, given a (general) fitted model and validation data, finds AMS for
#some value of theta. Useful for plotting and finding the maximum.

AMS <- function(f, valid_set, valid_y, valid_weights){

  AMS_theta <- function(theta){

             probabilities <- predict(f$finalModel,valid_set, type = "response")
             #mean(probabilities)

             predicted.classes <- ifelse(probabilities > theta, 1, 0)
             Label_valid <-  as.array(unlist(valid_y))
             #levels(Label_valid)
             Label_valid <- as.numeric(levels(Label_valid))[Label_valid] #convert from factor to numeric

             s <- sum(weights_Valid[(Label_valid == 1) & (predicted.classes == 1)])
             b <- sum(weights_Valid[(Label_valid == 0) & (predicted.classes == 1)])

             sqrt(2*((s+b+10)*log(1+s/(b+10))-s))

         }

     }

#Plot AMS for small values of threshold theta

theta_vals <- as.data.frame(seq(0.0001, 0.05, length.out=500)) # generate small sample thresholds theta
AMS_vals <- apply(theta_vals, 1, AMS(logreg_weighted,Valid[,1:30],Valid[31], weights_Valid)) #compute AMS(theta)
plot(as.array(unlist(theta_vals)), AMS_vals, xlab="theta", ylab="AMS(theta)", pch=19) #plot it

max_theta <- theta_vals[which.max(AMS_vals),1]
max_AMS <- AMS_vals[which.max(AMS_vals)]
max_AMS

# sensitivity(
#   data = as.factor(predicted.classes),
#   reference = as.array(unlist(Valid[,31])),
#   positive = levels(as.array(unlist(Valid[,31])))[2]
# )


#For the weighted logistic regression, AMS is decreasing with theta, for \theta \in ]0,1[



# pred_tibble <- tibble("target" = test_pfi,
#                       "prediction" = predicted.classes)
# table <- as_tibble(table(pred_tibble))
#
# plot_confusion_matrix(table,
#                       target_col = "target",
#                       prediction_col = "prediction",
#                       counts_col = "n")



#####################################################################################

# Cross-Validation function for choosing threshold

threshold_CV <- function(df, label, weights, theta_0, theta_1, k=5, n=50){

  N_s <- sum(weights[label == 1])
  N_b <- sum(weights[label == 0])

  train_control <- trainControl(method = "cv", number = 2)
  theta_vals <- as.data.frame(seq(theta_0, theta_1, length.out=n))
  AMS_vals <- matrix(0, nrow=n, ncol=k)

  validFolds <- createFolds(label, k)
  for (i in 1:k){

    Train <- df[-validFolds[[i]],]
    Train$Label <- label[-validFolds[[i]]]
    Train_weights <- reweight(weights[-validFolds[[i]]], Train$Label, N_s=N_s, N_b=N_b)

    Valid  <- df_train[validFolds[[i]],]
    Valid$Label <- label[validFolds[[i]]]
    Valid_weights <- reweight(weights[validFolds[[i]]], Valid$Label, N_s=N_s, N_b=N_b)

    logreg_weighted <- caret::train(Label ~ .,
                                  data = Train,
                                  trControl = train_control,
                                  method = "glm",
                                  metric="sensitivity",
                                  weights = Train_weights,
                                  family=binomial()
    )
    AMS_vals[,i] <- apply(theta_vals, 1, AMS(logreg_weighted, Valid[,-length(Valid)], Valid$Label, Valid_weights))
  }
  AMS_vals <- apply(AMS_vals, 1, mean)
  max_theta <- theta_vals[which.max(AMS_vals),1]
  max_AMS <- AMS_vals[which.max(AMS_vals)]
  return(c(max_theta=max_theta, max_AMS=max_AMS, AMS_vals=AMS_vals))
}

theta_CV <- threshold_CV(df_train[,1:30], df_train$Label, weights, theta_0=0.0001, theta_1=0.02)
