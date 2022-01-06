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
#library(higgsboson)

######################################################

# Useful variables

#train <- higgsboson::training

load('./data/training.RData')
train <- training
rm(training)

df_train <- train[,2:33] #remove eventid
df_train <- df_train[,-31] #remove weights

df_train$Label=ifelse(df_train$Label=="s",1,0) #encode "s" and "b" to 1 - 0 (resp.) for logistic regresion

label_factor=as.factor(df_train$Label)
df_train["Label"]=label_factor #need this as factor for caret

weights <- train$Weight # extract weights

# What to do with missing values? ==============================================

# df_train has 177457 observations for which all 'PRI_jet_subleading...' variables
# (and corresponding derived variables) are missing.
# Of these observations, 99913 also lack 'PRI_jet_leading...' variables.
# (No observations have missing 'leading' variables but complete 'subleading' variables)

# 38114 observations have missing 'DER_mass_MMC'.

df_train %>% summarise(across( !Label, ~sum(.==-999))) %>% as_vector()

df_train %>% filter(DER_mass_MMC==-999) %>%
  summarise(across( !Label, ~sum(.==-999))) %>% as_vector()

# So there are 3 types of incomplete observations:
# - 'DER_mass_MMC' missing
# - 'PRI_jet_subleading...' missing
# - 'PRI_jet_leading...' missing (a subset of 'PRI_jet_subleading...' missing)

# The last two types (and possibly also 'DER_mass_MMC') correspond to decay paths
# for which the missing variables are physically meaningless. So imputation probably
# isn't helpful (or at least isn't motivated by the physics).

# There are 3 possible ways of changing df_train to deal with missingness below
# running

# # 1. Set missing values (currently -999) to zero
# df_train <- df_train %>%
#   mutate(across( !Label, function(x) if_else(x==-999, 0, x) ))

# # 2. Set missing values (currently -999) to column mean
# df_train <- df_train %>%
#   mutate(across( !Label, ~na_if(.,-999) ))
# colmeans <- df_train %>%
#   summarise(across( !Label , function(x)mean(x, na.rm=TRUE) ))
# for(i in 1:30){
#   df_train[[i]] <- replace_na(df_train[[i]], colmeans[[i]])
# }

# # 3. Set flag variables for missingness
# df_train <- df_train %>%
#   mutate( FLAG_mass_MMC = as.numeric(DER_mass_MMC==-999),
#           FLAG_subleading = as.numeric(PRI_jet_subleading_pt==-999),
#           FLAG_leading = as.numeric(PRI_jet_leading_pt==-999),
#           .before = Label)

# None of these seem to make much difference! Using the model below
# (copied from Weighted_logistic_regression.R) the AMS values are similar,
# although the peaks are shifted in theta.

# I don't know if there are other (better) ways of dealing with missingness,
# or whether they would make more difference with other models. Based on
# the (very unsophisticated) approaches here it seems we probably don't have to
# worry

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

# function for reweighting
reweight <- function(weights, labels, N_s, N_b){
  new_weights <- weights
  new_weights[labels == 1] <- weights[labels == 1] * N_s / sum(weights[labels == 1])
  new_weights[labels == 0] <- weights[labels == 0] * N_b / sum(weights[labels == 0])
  return(new_weights)
}

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

theta_vals <- as.data.frame(runif(100, 0.0001, 0.05)) # generate small sample thresholds theta
AMS_vals <- apply(theta_vals, 1, AMS(logreg_weighted,select(Valid,!Label),Valid['Label'], weights_Valid)) #compute AMS(theta)
plot(as.array(unlist(theta_vals)), AMS_vals, xlab="theta", ylab="AMS(theta)", pch=19) #plot it
