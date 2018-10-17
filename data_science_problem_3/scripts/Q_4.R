# profit generation model


#big_case_train <- read.csv("~/Downloads/Puzzle/big_case_train.csv")
big_case_train <- read.csv("C:/Users/sidharth.suman/Desktop/hacktemp/big_case_train.csv")
train_dataset_without_missing_target<- read.csv('C:/Users/sidharth.suman/Desktop/hacktemp/train_dataset_without_missing_target.csv')

#checking for messy data
which(big_case_train$spends>big_case_train$credit_line)
which(is.na(big_case_train)==T)

# filter out customers who defaulted using the train data
train_dataset_without_missing_target <- train_dataset_without_missing_target[train_dataset_without_missing_target$default==F,]

# then get the profit of remaining cusotmers and only keep those customers assuming others defaulted
big_case_train$profit<- (big_case_train$spends*0.05)+(big_case_train$revolving_balance*0.17)
big_case_train <- big_case_train[big_case_train$ids %in% train_dataset_without_missing_target$ids,]

# getting total profit by each customer up till today
library(stats)
profit_per_customer<- data.frame(aggregate(big_case_train$profit, by=list(big_case_train$ids), FUN=sum))
colnames(profit_per_customer)<-c('ids','profit')

# then add this profit as parameter to train data of non-default cutomers and remove default and run linear regresison
train_dataset_profit<- merge(train_dataset_without_missing_target, profit_per_customer, by="ids")
train_dataset_profit$default <- NULL

library(dplyr)
clean_data <- select(train_dataset_profit,-c(ids))
summary(clean_data$profit)
train<-sample_frac(clean_data,0.7)
rowid<-as.numeric(rownames(train)) # because rownames() returns character
test<-clean_data[-rowid,]
set.seed(100)
# regression
linear_regression_profit<- lm(profit~.,train)
coefficients <- linear_regression_profit$coefficients
index <- which(names(test)=='profit')
test$predicted_profit<-predict(linear_regression_profit,test[,-index])
options(scipen = 999)
summary(linear_regression_profit)


library(Metrics)
rsquared <- function(actual,preds){
  rss <- sum((preds - actual) ^ 2)  ## residual sum of squares
  tss <- sum((actual - mean(actual)) ^ 2)  ## total sum of squares
  rsq <- 1 - rss/tss
  return(rsq)
}
regression_model_eval<-function(actual,pred){
  options(scipen = 999)
  cat('MSE ')
  cat(mse(actual,pred))
  cat('\n')
  cat('rmse ')
  cat(rmse(actual,pred))
  cat('\n')
  cat('mape ')
  cat(mape(actual,pred))
  cat('\n')
  cat('mae ')
  cat(mae(actual,pred))
  cat('\n')
  cat('rsquared ')
  cat(rsquared(actual,pred))
  
}
regression_model_eval(test$profit,test$predicted_profit)
save(linear_regression_profit,file = 'C:/Users/sidharth.suman/Desktop/hacktemp/linear_regression_profit.rda')


library(caret)
train.control <- trainControl(method = "cv", number = 10)
# Stepwise regression model
step.model <- train(profit ~., data = train,
                    method = "lmStepAIC", 
                    trControl = train.control,
                    trace = FALSE
)
test$predicted_profit<- predict(step.model,test[,-c(29,30)])
summary(step.model)
regression_model_eval(test$profit,test$predicted_profit)
