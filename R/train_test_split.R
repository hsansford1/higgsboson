# data in kaggle folder is taken from the kaggle challenge page,
# but their testing data (./kaggle/kaggle_test.csv) doesn't contain
# the target variable

# ./atlas-higgs-challenge-2014-v2.csv is 'master' dataset,
# and contains all fields for all events, including whether kaggle 
# assigned each event as a training or testing sample

# this script splits data into 250000 training and 550000 testing samples,
# corresponding to kaggle challenge training/testing files but containing
# fields

# feel free to change this train/test split, but I thought it would be sensible
# follow the kaggle scheme, at least to start
# (we can also evaluate our model according to kaggle's scoring system, so using
# the same train/test split allows us to compare scores with other users)

# nb: there are 18238 unused samples (KaggleSet == 'u')

library(tidyverse)

data <- read_csv('./data/atlas-higgs-challenge-2014-v2.zip')

training <- filter(data, KaggleSet == 't')
training <- select(training, -c('KaggleSet', 'KaggleWeight'))
save(training, file = './data/training.RData')

test <- filter(data, (KaggleSet == 'b') | (KaggleSet == 'v'))
test <- select(test, -c('KaggleSet', 'KaggleWeight'))
save(test, file = './data/test.RData')
