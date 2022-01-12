load('./data/training.RData')
load('./data/test.RData')
source('./R/useful_functions.R')

library(comprehenr)

df_train <- training[,2:33] #remove eventid
df_train <- df_train[,-31] #remove weights

# Need to make sure missing values are removed and data is standaerdised for RBF transform
df_train[df_train==-999] <- 0
st_train <- as.data.frame(scale(df_train[,1:30]))

df_test <- test[,2:33] #remove eventid
df_test <- df_test[,-31] #remove weights

df_train$Label <- ifelse(df_train$Label=="s",1,0) #encode "s" and "b" to 1 - 0 (resp.)
df_train$Label <- as.factor(df_train$Label) #need this as factor for caret

df_test$Label <- ifelse(df_test$Label=="s",1,0)
df_test$Label <- as.factor(df_test$Label)

train_weights <- reweight(training$Weight, df_train$Label, Ns(), Nb()) # extract weights


# create RBF function for given centroid
rbf.func <- function(centroid, sigmasq){
  rbf <- function(x){ return (exp(-sum((x - centroid)^2) / (2*sigmasq)))}
  return(rbf)
}

# randomly select n centroids from dataframe
get_centroids <- function(df, n){
  indices <- sample.int(dim(df)[1], n)
  centroids <- to_list(for (i in indices) df[i,])
  return(centroids)
}

# Compute new dataframe with n extra RBF transform columns
compute_phiX <- function(X, n, sigmasq){
  centroids <- get_centroids(X, n)
  phiX <- X
  for (i in 1:n){
    rbf <- rbf.func(centroids[[i]], sigmasq)
    phiX[paste0('RBF', i)] <- apply(X, 1, rbf)
  }
  return(phiX)
}

# function is quick for 1000 datapoints and 2 extra transforms, but takes too long to run for our huge dataframe...
compute_phiX(st_train[1:1000,], 2, 1)
