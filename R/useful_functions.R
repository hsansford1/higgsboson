


# function for reweighting
reweight <- function(weights, labels, N_s, N_b){
  new_weights <- weights
  new_weights[labels == 1] <- weights[labels == 1] * N_s / sum(weights[labels == 1])
  new_weights[labels == 0] <- weights[labels == 0] * N_b / sum(weights[labels == 0])
  return(new_weights)
}




#-----------------------------------------------------------------

# AMS for tuning threshold

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




# CV for stage 2

threshold_CV <- function(df, label, weights, theta_0, theta_1, k, n=50){
  
  N_s <- sum(weights[label == 1])
  N_b <- sum(weights[label == 0])
  
  train_control <- trainControl(method = "cv", number = 2)
  theta_vals <- as.data.frame(seq(theta_0, theta_1, length.out=n))
  AMS_vals <- matrix(0, nrow=n, ncol=k)
  
  validFolds <- createFolds(label, k)
  for (i in 1:k){
    
    Train <- df[-validFolds[[i]],]
    Train$Label <- label[-validFolds[[i]]]
    Train_weights <- reweight(weights[-validFolds[[i]]], Train$Label, N_s, N_b)
    
    Valid  <- df_train[validFolds[[i]],]
    Valid$Label <- label[validFolds[[i]]]
    Valid_weights <- reweight(weights[validFolds[[i]]], Valid$Label, N_s, N_b)
    
    logreg_weighted <- caret::train(Label ~ .,
                                    data = Train,
                                    trControl = train_control,
                                    method = "glm",
                                    metric="sensitivity",
                                    weights = Train_weights,
                                    family=binomial()
    )
    AMS_vals[,i] <- apply(theta_vals, 1, AMS(logreg_weighted, Valid[,1:30], Valid$Label, Valid_weights))
  }
  AMS_vals <- apply(AMS_vals, 1, mean)
  max_theta <- theta_vals[which.max(AMS_vals),1]
  max_AMS <- AMS_vals[which.max(AMS_vals)]
  return(c(max_theta=max_theta, max_AMS=max_AMS, AMS_vals=AMS_vals))
}






#---------------------------------------------------------------------

# mlr package: AMS as direct measure

AMS_weighted = function(truth, response) {
  
  s <- sum(weights[(truth == 1) & (response == 1)])
  b <- sum(weights[(truth == 0) & (response == 1)])
  
  sqrt(2*((s+b+10)*log(1+s/(b+10))-s)) #calculate AMS
  
}


AMS_mlr = makeMeasure(
  id = "AMS_weighted", minimize = FALSE,
  properties = c("classif"),
  name = "Approximate median significance",
  fun = function(task, model, pred, feats, extra.args) {
    AMS_weighted(pred$data$truth, pred$data$response)
  }
)

