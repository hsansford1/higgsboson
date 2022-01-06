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

AMS_weighted = function(truth, response) {

  s <- sum(weights[(truth == 1) & (response == 1)])
  b <- sum(weights[(truth == 0) & (response == 1)])

  sqrt(2*((s+b+10)*log(1+s/(b+10))-s)) #calculate AMS

}


AMS = makeMeasure(
  id = "AMS_weighted", minimize = FALSE,
  properties = c("classif"),
  name = "Approximate median significance",
  fun = function(task, model, pred, feats, extra.args) {
    AMS_weighted(pred$data$truth, pred$data$response)
  }
)



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
                        measures = AMS,
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

s <- sum(weights[(truth == 1) & (response == 1)])
b <- sum(weights[(truth == 0) & (response == 1)])

AMS <- sqrt(2*((s+b+10)*log(1+s/(b+10))-s))
AMS


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

weights_Train <- weights[trainIndex]#I think we must recompute weights...
# reweighting
weights_Train[Train$Label == 1] <- weights_Train[Train$Label == 1] * (N_s / sum(weights_Train[Train$Label == 1]))
weights_Train[Train$Label == 0] <- weights_Train[Train$Label == 0] * (N_b / sum(weights_Train[Train$Label == 0]))

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

AMS <- function(f, valid_set, valid_y){

  AMS_theta <- function(theta){

             probabilities <- predict(f$finalModel,valid_set, type = "response")
             #mean(probabilities)

             predicted.classes <- ifelse(probabilities > theta, 1, 0)
             Label_valid <-  as.array(unlist(valid_y))
             #levels(Label_valid)
             Label_valid <- as.numeric(levels(Label_valid))[Label_valid] #convert from factor to numeric

             confusion_table <- table(predicted.classes, Label_valid)
             s  <- confusion_table[2,2]/(confusion_table[2,2]+confusion_table[1,2]) #TPR - sensitivity
             b  <- confusion_table[2,1]/(confusion_table[2,1]+confusion_table[1,1])

             sqrt(2*((s+b+10)*log(1+s/(b+10))-s))

         }

     }

#Plot AMS for small values of threshold theta

theta_vals <- as.data.frame(runif(100, 0.0001, 0.5)) # generate small sample thresholds theta
AMS_vals <- apply(theta_vals, 1, AMS(logreg_weighted,Valid[,1:30],Valid[31])) #compute AMS(theta)
plot(as.array(unlist(theta_vals)), AMS_vals, xlab="theta", ylab="AMS(theta)", pch=19) #plot it



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









