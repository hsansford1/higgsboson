library(mlr)

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
threshold_CV <- function(df, label, weights, theta_0, theta_1, k=5, n=200){

  theta_vals <- as.data.frame(seq(theta_0, theta_1, length.out=n))
  max_thetas <- rep(0,k)
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
                                    # metric="sensitivity",
                                    weights = weights_Train,
                                    family=binomial()
    )

    AMS_vals[,i] <- apply(theta_vals, 1, AMS(logreg_weighted,Valid[,-length(Valid)],Valid[length(Valid)], weights_Valid))
    max_thetas[i] <- theta_vals[which.max(AMS_vals[,i]),1]
  }
  AMS_mean <- apply(AMS_vals, 1, mean)
  AMS_sd <- apply(AMS_vals, 1, sd)
  plot(as.array(unlist(theta_vals)), AMS_mean, xlab="theta", ylab="AMS(theta)", pch=19)
  lines(as.array(unlist(theta_vals)), AMS_mean + mean(AMS_sd), col='red')
  lines(as.array(unlist(theta_vals)), AMS_mean - mean(AMS_sd), col='red')
  max_theta <- theta_vals[which.max(AMS_mean),1]
  max_AMS <- AMS_mean[which.max(AMS_mean)]
  return(list('max_theta'=max_theta, 'max_AMS'=max_AMS, 'AMS_sd'=AMS_sd, 'max_thetas'=max_thetas))
}

#---------------------------------------------------------------------

#' Approximate Median Significance (AMS) function using weights of data points
#'
#' @param truth The true labels (0/1) of the data points.
#' @param response The predicted labels (0/1) of the data points.
#' @param weights The weights of the data points (can be un-normalized)
#'
#' @return AMS.
#' @export
AMS_weighted = function(truth, response, weights) {

  weights <- reweight(weights, truth, Ns(), Nb())
  s <- sum(weights[(truth == 1) & (response == 1)])
  b <- sum(weights[(truth == 0) & (response == 1)])

  return(AMS_base(s, b))

}



#' Generate AMS measure to be used in mlr training
#'
#' @return AMS measure that can be used as a measure in functions in the `mlr` package, e.g. `crossval`
#' @import mlr
AMS_measure <- function(){
  library(mlr)
  AMS_mlr = mlr::makeMeasure(
    id = "AMS_weighted", minimize = FALSE,
    properties = c("classif"),
    name = "Approximate median significance",
    fun = function(task, model, pred, feats, extra.args) {
      AMS_weighted(pred$data$truth, pred$data$response, weights)
    }
  )
  return(AMS_mlr)
}

