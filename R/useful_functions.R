# Hardcode Ns, Nb as functions returning the desired values
# (see train_test_split.R for explanation)
Ns <- function(){return(691.988607711987)}
Nb <- function(){return(410999.847321811)}

# function for reweighting
reweight <- function(weights, labels, N_s, N_b){

  # use hardcoded Ns, Nb
  N_s <- Ns()
  N_b <- Nb()
  # if using hardcoded Ns, Nb makes AMS values work,
  # remove N_s, N_b function arguments later

  new_weights <- weights
  new_weights[labels == 1] <- weights[labels == 1] * N_s / sum(weights[labels == 1])
  new_weights[labels == 0] <- weights[labels == 0] * N_b / sum(weights[labels == 0])
  return(new_weights)
}

AMS_base <- function(s, b, b_reg=10){

  # Objective function for challenge: approximate median significance
  # (eqn 7 in https://higgsml.lal.in2p3.fr/files/2014/04/documentation_v1.8.pdf)

  # Takes luminosity-normalised true and false positive rates
  # (s and b respectively) and regularisation term b_reg (=10 by default),
  # returns real-valued AMS, or complains

  AMS_sq <- (s+b+b_reg) * log(1 + (s/(b+b_reg))) - s
  if(AMS_sq < 0){ stop('AMS squared is negative') }
  return( sqrt(AMS_sq) )
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

    s <- sum(valid_weights[(Label_valid == 1) & (predicted.classes == 1)])
    b <- sum(valid_weights[(Label_valid == 0) & (predicted.classes == 1)])

    return(AMS_base(s, b))

  }

}




# CV for stage 2

threshold_CV <- function(df, label, weights, theta_0, theta_1, k, n=50){

  N_s <- Ns()
  N_b <- Nb()

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
    AMS_vals[,i] <- apply(theta_vals, 1, AMS(logreg_weighted, Valid[,1:30], Valid$Label, Valid_weights))
  }
  AMS_vals <- apply(AMS_vals, 1, mean)
  max_theta <- theta_vals[which.max(AMS_vals),1]
  max_AMS <- AMS_vals[which.max(AMS_vals)]
  return(c(max_theta=max_theta, max_AMS=max_AMS, AMS_vals=AMS_vals))
}

#---------------------------------------------------------------------

# mlr package: AMS as direct measure

AMS_weighted = function(truth, response, weights) {

  weights <- reweight(weights, truth, Ns(), Nb())
  s <- sum(weights[(truth == 1) & (response == 1)])
  b <- sum(weights[(truth == 0) & (response == 1)])

  return(AMS_base(s, b))

}

# Think it might be better to document functions that labels should be 0/1 because then our functions are more general for binary problems
AMS_weighted_gen = function(truth, response, weights) {
# more general version of AMS_weighted,
# takes truth (actual outcome), response (predicted outcome), and weight vector
# returns AMS
  weights <- reweight(weights, truth, Ns(), Nb())
# truth & response can be s/b or 1/0
# both vectors should be same type
  if((truth[1] == 's') | (truth[1] == 'b')){
    s <- sum(weights[(truth == 's') & (response == 's')])
    b <- sum(weights[(truth == 'b') & (response == 's')])
  } else if((truth[1] == 1) | (truth[1] == 0)){
    s <- sum(weights[(truth == 1) & (response == 1)])
    b <- sum(weights[(truth == 0) & (response == 1)])
  }

  return(AMS_base(s, b))

}


AMS_mlr = makeMeasure(
  id = "AMS_weighted", minimize = FALSE,
  properties = c("classif"),
  name = "Approximate median significance",
  fun = function(task, model, pred, feats, extra.args) {
    AMS_weighted(pred$data$truth, pred$data$response, weights)
  }
)

