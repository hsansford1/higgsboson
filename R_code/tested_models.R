# Load test set


test <- higgsboson::test

df_test <- test[,2:33] #remove eventid
df_test <- df_test[,-31] #remove weights
df_test$Label <- ifelse(df_test$Label=="s",1,0) #encode "s" and "b" to 1 - 0 (resp.) for logistic regresion
df_test$Label <- as.factor(df_test$Label) #need this as factor for caret

weights_test <- reweight(test$Weight, df_test$Label, Ns(), Nb()) # extract weights

# add a test in for weights?
all.equal(sum(weights_test[df_test$Label == 1]), Ns())
all.equal(sum(weights_test[df_test$Label == 0]), Nb())

######################################################################

#Missing values (and standardisation)

df_test[df_test==-999] <- 0
st_test <- as.data.frame(scale(df_test[,1:30])) #standardisation
st_test$Label <- as.factor(df_test$Label)

######################################################################



# Test model from simple weighted logistic regression (theta=0.5)

logreg_weighted2 <- caret::train(Label ~ .,
                                 data = st_train,
                                 method = "glm",
                                 #metric="sensitivity",
                                 weights = weights,
                                 family=binomial()
)

probabilities <- predict(logreg_weighted2$finalModel, st_test[,1:30], type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

Label_valid <-  as.array(unlist(st_test[,31]))
Label_valid <- as.numeric(levels(Label_valid))[Label_valid] #convert from factor to numeric

s <- sum(weights_test[(Label_valid == 1) & (predicted.classes == 1)])
b <- sum(weights_test[(Label_valid == 0) & (predicted.classes == 1)])
AMS_base(s, b)  #AMS of about 0.18 -low



##############################################################################

# Test model from two-stage procedure: use model before but
# predict "s" for probabilities > optimal theta

theta = 0.0056 #optimal theta. On train AMS was around 1.5 (1.53)

probabilities <- predict(logreg_weighted2$finalModel, st_test[,1:30], type = "response")

predicted.classes <- ifelse(probabilities > theta, 1, 0)

Label_valid <-  as.array(unlist(st_test[,31]))
Label_valid <- as.numeric(levels(Label_valid))[Label_valid] #convert from factor to numeric

s <- sum(weights_test[(Label_valid == 1) & (predicted.classes == 1)])
b <- sum(weights_test[(Label_valid == 0) & (predicted.classes == 1)])
AMS_base(s, b) #AMS of 1.55 - good



##############################################################################

# theta suggested from PCA

theta_pca = 0.0017 #optimal theta pca. On train AMS was around 1.2


probabilities <- predict(logreg_weighted2$finalModel, st_test[,1:30], type = "response")

predicted.classes <- ifelse(probabilities > theta_pca, 1, 0)

Label_valid <-  as.array(unlist(st_test[,31]))
Label_valid <- as.numeric(levels(Label_valid))[Label_valid] #convert from factor to numeric

s <- sum(weights_test[(Label_valid == 1) & (predicted.classes == 1)])
b <- sum(weights_test[(Label_valid == 0) & (predicted.classes == 1)])
AMS_base(s, b) #AMS of 1.37 -  good but smaller than before. Possible small overfitting


#############################################################################


##############################################

#bad practice: 

theta_vals_test <- seq(0.0001, 1, length.out=500) # generate small sample thresholds theta
AMS_test <- rep(0,500)

for (i in 1:500) {
  
  probabilities <- predict(logreg_weighted2$finalModel, st_test[,1:30], type = "response")
  
  predicted.classes <- ifelse(probabilities > theta_vals_test[i], 1, 0)
  
  s <- sum(weights_test[(Label_valid == 1) & (predicted.classes == 1)])
  b <- sum(weights_test[(Label_valid == 0) & (predicted.classes == 1)])
  AMS_test[i] <- AMS_base(s, b)
}

plot(as.array(theta_vals_test), AMS_test, xlab="theta", ylab="AMS(theta)", pch=19) #plot it

theta_max_test <- theta_vals_test[which.max(AMS_test)]
theta_max_test


##############################################################