library(tidyverse)
library(reshape2)
library(ggplot2)
library(glmnet)
library(ROCR)
library(caret)
library(cvms)
library(tibble) 
library(pROC)

library(devtools)
install_github("https://github.com/hsansford1/higgsboson")
library(higgsboson)


train <- higgsboson::training

df_train <- train[,2:33] #remove eventid
df_train <- df_train[,-31] #remove weights


df_train$Label=ifelse(df_train$Label=="s",1,0) #encode "s" and "b" to 1 - 0 (resp.) for logistic regresion
label_factor=as.factor(df_train$Label)
df_train["Label"]=label_factor #need this as factor for caret

weights <- train$Weight # extract weights

train_control <- trainControl(method = "cv", number = 10) 
# !!! prob need to modify cv to AMS metric !!!

model_weights <- train(Label ~ .,
                       data = df_train,
                       trControl = train_control,
                       method = "glm",
                       metric="balanced accuracy",
                       weights = weights,
                       family=binomial()
)

print(model_weights)
confusionMatrix(model_weights)

# print confusion matrix, sens. and spec.
cm<- confusionMatrix(model_weights)
plt <- as.data.frame(round(cm$table,1))
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))

ggplot(plt, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Prediction",y = "Reference") 



# Correlation heatmap
cormat <- round(cor(df_train[,-31]),2)
library(reshape2)
melted_cormat <- melt(cormat) #only DER variables

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()
