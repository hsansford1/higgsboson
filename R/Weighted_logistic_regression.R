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
AMS_mlr
cv.logistic <- crossval(learner = logistic.learner, task = trainTask, iters = 5 ,
                        stratify = FALSE,
                        measures = AMS_mlr,
                        show.info = F)
cv.logistic$aggr   # If we do it with no weights AMS is larger...
cv.logistic$measures.test

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

AMS <- sqrt(2*((s+b+10)*log(1+s/(b+10))-s))
AMS
# XXX

#####################################################################

# Two-stage maximisation of AMS: Kotlowsky's paper

#Idea: learn a model (e.g. log reg) f
#calibrate classification threshold theta by maximising \hat(AMS)(theta) on a validation set

#create the validation set (caret). This should preserve the overall class distribution of the data

trainIndex <- createDataPartition(df_train$Label, p = .8, list = FALSE, times = 1)
head(trainIndex)

Train <- df_train[ trainIndex,]
Valid  <- df_train[-trainIndex,]


#Logistic regression on Train: CV and sensitivity as metric

weights_Train <- reweight(weights[trainIndex], Train$Label, Ns(), Nb())
weights_Valid <- reweight(weights[-trainIndex], Valid$Label, N_s, N_b)


#train_control <- trainControl(method = "cv", number = 10)

logreg_weighted2 <- caret::train(Label ~ .,
                       data = Train,
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

#Plot AMS for small values of threshold theta

theta_vals <- as.data.frame(seq(0.0001, 0.05, length.out=500)) # generate small sample thresholds theta
AMS_vals <- apply(theta_vals, 1, AMS(logreg_weighted2,Valid[,1:30],Valid[31], weights_Valid)) #compute AMS(theta)
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

threshold_CV <- function(df_train, weights, theta_0, theta_1, k=5, n=50){

  theta_vals <- as.data.frame(seq(theta_0, theta_1, length.out=n))
  AMS_vals <- matrix(0, nrow=n, ncol=k)

  for (i in 1:k){

    trainIndex <- createDataPartition(df_train$Label, p = .8, list = FALSE, times = 1)

    Train <- df_train[ trainIndex,]
    Valid  <- df_train[-trainIndex,]

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
  }
  AMS_vals <- apply(AMS_vals, 1, mean)
  plot(as.array(unlist(theta_vals)), AMS_vals, xlab="theta", ylab="AMS(theta)", pch=19)
  max_theta <- theta_vals[which.max(AMS_vals),1]
  max_AMS <- AMS_vals[which.max(AMS_vals)]
  return(c(max_theta=max_theta, max_AMS=max_AMS))
}

theta_CV <- threshold_CV(df_train, weights, theta_0=0.0001, theta_1=0.02)
theta_CV
