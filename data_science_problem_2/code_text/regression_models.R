##############################################################################
#Project : Adidas regression models
#
#Date - 5th June 2018
##############################################################################

library(readxl)
library(Metrics)
library(randomForest)
library(ggplot2)
library(miscTools)

normally_distribted <- read_excel("C:/Users/sidharth.suman/Desktop/adidas/datasets/normally_distribted.xlsx")
normally_distribted$X__1<-NULL

# Sample Indexes
set.seed(100)
indexes = sample(1:nrow(normally_distribted), size=0.7*nrow(normally_distribted))

# Split data
train_df = normally_distribted[indexes,-1]
dim(train_df) 
test_df = normally_distribted[-indexes,-1]
dim(test_df) 


############################################################################
# for ploting the coefficients of linear model
############################################################################

plot_coeffs_S <- function(mlr_model,label) {
  coeffs <- sort(coefficients(mlr_model), decreasing = TRUE)  ### changed
  
  mp <-barplot(coeffs, col="#3F97D0", xaxt='n', main=label)
  lablist <- names(coeffs)
  
  text(mp, par("usr")[3], labels = lablist, srt = 40, adj = c(.8,.8), xpd = TRUE,cex = .9)
}


#############################################################################
# Multiple linear regression   
#############################################################################

mult_reg <- lm(formula = `Page Views`~.,data = train_df)

coefficients <- mult_reg$coefficients
features <- names(coef(mult_reg))
imp_df<- as.data.frame(cbind(features,coefficients))
imp_df <- imp_df[order(coefficients,decreasing = T),]

plot_coeffs_S(mult_reg,'Regression Coefficients for Page Views')

index <- which(names(test_df)=='Page Views')
pred<-predict(mult_reg,test_df[,-index])

summary(mult_reg)
mse(test_df$`Page Views`,pred)
rmse(test_df$`Page Views`,pred)
mape(test_df$`Page Views`,pred)


#############################################################################
# Random forest
#############################################################################

new_name_train_data <- train_df
names(new_name_train_data) <- make.names(names(new_name_train_data))
rf <- randomForest(Page.Views ~ ., data = new_name_train_data,ntree =20)

new_name_test_data <- test_df
names(new_name_test_data) <- make.names(names(new_name_test_data))

pred <-predict(rf,new_name_test_data)

varImpPlot(rf,sort = T,main = 'variable importance for Revenue')


p <- ggplot(aes(x=actual,y=pred),
            data = data.frame(actual = new_name_test_data$Page.Views,pred = pred))
p + geom_point() +
  geom_abline(color = 'red') +
  ggtitle("Random Forest Regression for Page View")

rSquared(new_name_test_data$Page.Views,new_name_test_data$Page.Views - pred)
mse(new_name_test_data$Page.Views,pred)
rmse(new_name_test_data$`Page.Views`,pred)
mape(new_name_test_data$`Page.Views`,pred)

#############################################################################
# Ridge regression
#############################################################################
library(tidyverse)
library(broom)
library(glmnet)

y <- train_df$`Page Views`
x <- train_df[,-which(names(train_df)=='Page Views')] %>% data.matrix()
cv_fit <- cv.glmnet(x, y, alpha = 0)
opt_lambda <- cv_fit$lambda.min
plot(cv_fit)
fit <- glmnet(x, y, alpha = 0,lambda = opt_lambda)
summary(fit)

new_x <- test_df[,-which(names(test_df)=='Page Views')] %>% data.matrix()
new_y <- test_df$`Page Views`
pred <- predict(fit, s = opt_lambda, newx = new_x)

# R squared
rSquared(test_df$`Page Views`,test_df$`Page Views` - pred)
mse(test_df$`Page Views`,pred)
rmse(test_df$`Page Views`,pred)
mape(test_df$`Page Views`,pred)

############################################################################
# Gradient boosting
############################################################################

library(gbm)
df.boost=gbm(`Page Views` ~ . ,data = train_df,distribution = "gaussian",n.trees = 20000,
           shrinkage = 0.01, interaction.depth = 4)
summary(df.boost)
pred <- predict(df.boost,n.trees = 20000,test_df[,-9])
rSquared(test_df$`Page Views`,test_df$`Page Views` - pred)
mse(test_df$`Page Views`,pred)
rmse(test_df$`Page Views`,pred)
mape(test_df$`Page Views`,pred)

############################################################################
# Xgboost
############################################################################

library(xgboost)

model <- train(`Page Views` ~., data = train_df, method = "xgbTree",
  trControl = trainControl("cv", number = 10))

# Best tuning parameter
model$bestTune

# Make predictions on the test data
pred <- model %>% predict(test_df)

rSquared(test_df$`Page Views`,test_df$`Page Views` - pred)
mse(test_df$`Page Views`,pred)
rmse(test_df$`Page Views`,pred)
mape(test_df$`Page Views`,pred)
