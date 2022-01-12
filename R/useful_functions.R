
#' Hardcoded N_s
#' @description Hardcoded N_s for the higgsboson data.
#' @return Desired sum of weights corresponding to data points with labels equal to 1.
#' @export
Ns <- function(){return(691.988607711987)}

#' Hardcoded N_b
#' @description Hardcoded N_b for the higgsboson data.
#' @return Desired sum of weights corresponding to data points with labels equal to 0.
#' @export
Nb <- function(){return(410999.847321811)}

#' Re-weighting
#'
#' @description Re-weighting to preserve the sum of weights in each binary class
#'
#' @param weights Input weights (unnormalized).
#' @param labels Binary labels of the data points corresponding to the input weights (labels should be 0/1).
#' @param N_s Desired sum of weights corresponding to data points with labels equal to 1.
#' @param N_b Desired sum of weights corresponding to data points with labels equal to 1.
#'
#' @return Normalized weights
#' @export
reweight <- function(weights, labels, N_s, N_b){

  new_weights <- weights
  new_weights[labels == 1] <- weights[labels == 1] * N_s / sum(weights[labels == 1])
  new_weights[labels == 0] <- weights[labels == 0] * N_b / sum(weights[labels == 0])
  return(new_weights)
}

#' Approximate Median Significance (AMS) Base Function
#'
#' @param s Sum of weights corresponding to true positive data points.
#' @param b Sum of weights corresponding to false positive data points.
#' @param b_reg Regularization parameter.
#'
#' @return Approximate median significance.
#' @export
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

#' Generate Approximate Median Significance (AMS) function for specified threshold
#'
#' @description Generates an AMS function with threshold (`theta`) of a logistic regression model as input.
#'  Useful for tuning threshold as a second stage to fitting logistic regression model.
#' @param f Weighted logistic regression model (output of caret::train).
#' @param valid_set Validation data set for model `f`.
#' @param valid_y Binary labels of validation data (0/1).
#' @param valid_weights Weights of validation data.
#'
#' @return Function with one input `theta`, that returns AMS of validation set using model `f` with threshold `theta`.
#' @export
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

#' Cross-validation function for tuning threshold in logistic regression.
#'
#' @description Function that performs k-fold cross-validation in order to tune the threshold parameter in logistic regression.
#'  Fits a weighted logistic regression using randomized training/validation split, then find the the threshold parameter that
#'  maximises the approximate median significance (AMS).
#' @param df Data-frame to perform cross-validation on.
#' @param label Binary labels of data points in `df` (0/1).
#' @param weights Weights of data points in `df`.
#' @param theta_0 Lower bound of threshold parameter.
#' @param theta_1 Upper bound of threshold parameter.
#' @param k Number of cross-validation sets.
#' @param n Number of values of threshold to check.
#'
#' @return `max_theta` is the threshold value that maximizes the AMS and `max_AMS` is the maximum average AMS found across cross-validation sets.
#' @export
#'
#' @examples
threshold_CV <- function(df, label, weights, theta_0, theta_1, k=5, n=50){

  theta_vals <- as.data.frame(seq(theta_0, theta_1, length.out=n))
  AMS_vals <- matrix(0, nrow=n, ncol=k)

  for (i in 1:k){

    trainIndex <- createDataPartition(df_train$Label, p = .8, list = FALSE, times = 1)

    Train <- df[ trainIndex,]
    Train$Label <- label[trainIndex]
    Valid  <- df[-trainIndex,]
    Valid$Label <- label[-trainIndex]

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

