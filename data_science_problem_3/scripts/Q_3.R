# model creation and training

#write.csv(train_dataset_without_missing_target,'C:/Users/sidharth.suman/Desktop/hacktemp/train_dataset_without_missing_target.csv',row.names = F)
#train_dataset_without_missing_target<- read.csv('C:/Users/sidharth.suman/Desktop/hacktemp/train_dataset_without_missing_target.csv')
library(dplyr)
check<-train_dataset_without_missing_target[which(train_dataset_without_missing_target$n_issues!=train_dataset_without_missing_target$n_accounts),]
clean_data <- select(train_dataset_without_missing_target,-c(ids))
clean_data$default <- factor(clean_data$default)
table(clean_data$default)

train<-sample_frac(clean_data,0.7)
rowid<-as.numeric(rownames(train)) # because rownames() returns character
test<-clean_data[-rowid,]

#table(train$default)
#library(DMwR)
#train <- SMOTE(default ~ ., train, perc.over = 700,perc.under=130)
#train$default <- factor(train$default)
#table(train$default)

# logistic regression
check_logit <- glm(default ~ ., data = train, family = "binomial")
summary(check_logit)

test$predicted_default <- predict(check_logit, newdata = select(test,-c(default)), type = "response")
test[test$predicted_default<=0.5,'predicted_default']<-0
test[test$predicted_default>0.5,'predicted_default']<-1
check_results<- data.frame(cbind(as.numeric(as.character(test$default)),test$predicted_default))
colnames(check_results) <- c('Actual','Predicted')
#save(check_logit,file = '~/Downloads/Puzzle/logistic_regression.rda')
save(check_logit,file = 'C:/Users/sidharth.suman/Desktop/hacktemp/logistic_regression.rda')

library(Metrics)

measure_model_metrics <- function(Actual,Predicted){
  accurate <- accuracy(Actual,Predicted)
  precision <- sum(Predicted & Actual) / sum(Predicted)
  recall <- sum(Predicted & Actual) / sum(Actual)
  fmeasure <- 2 * precision * recall / (precision + recall)
  
  cat('accuracy:   ')
  cat(accurate * 100)
  cat('%')
  cat('\n')
  
  cat('precision:  ')
  cat(precision * 100)
  cat('%')
  cat('\n')
  
  cat('recall:     ')
  cat(recall * 100)
  cat('%')
  cat('\n')
  
  cat('f-measure:  ')
  cat(fmeasure * 100)
  cat('%')
  cat('\n')
  
}

measure_model_metrics(check_results$Actual,check_results$Predicted)
table(check_results$Actual,check_results$Predicted)

#SVM

library(e1071)
model_svm <- svm(default ~ . , train)
pred <- predict(model_svm, train[,-1])

#For svm, we have to manually calculate the difference between actual values (train$y) with our predictions (pred)

error <-as.numeric(as.character(train$default)) - as.numeric(as.character(pred))
svm_error <- sqrt(mean(error^2))
test$predicted_default<-NULL
pred <- predict(model_svm, test[,-1])
measure_model_metrics(as.numeric(as.character(test$default)),as.numeric(as.character(pred)))
#save(model_svm,file = '~/Downloads/Puzzle/svm.rda')
save(model_svm,file = 'C:/Users/sidharth.suman/Desktop/hacktemp/svm.rda')

#Random forest
library(randomForest)
# Fitting model
fit <- randomForest(default ~ ., train,ntree=50)
summary(fit)
#Predict Output 
predicted<- predict(fit,test[,-1])
measure_model_metrics(as.numeric(as.character(test$default)),as.numeric(as.character(predicted)))
#save(fit,file = '~/Downloads/Puzzle/random_forest.rda')
save(fit,file = 'C:/Users/sidharth.suman/Desktop/hacktemp/random_forest.rda')
#library(reprtree)
#plot.getTree(fit, 1, labelVar=TRUE)
varImpPlot(fit,type=2)

#Boosting
library(caret)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
# C5.0
set.seed(seed)
fit.c50 <- train(default~., data=train, method="C5.0", metric=metric, trControl=control)
predicted<- predict(fit.c50,test[,-1])
measure_model_metrics(as.numeric(as.character(test$default)),as.numeric(as.character(predicted)))
save(fit.c50,file = 'C:/Users/sidharth.suman/Desktop/hacktemp/boosting_c5-0.rda')
# Stochastic Gradient Boosting
set.seed(seed)
fit.gbm <- train(default~., data=train, method="gbm", metric=metric, trControl=control, verbose=FALSE)
predicted<- predict(fit.gbm,test[,-1])
measure_model_metrics(as.numeric(as.character(test$default)),as.numeric(as.character(predicted)))
save(fit.gbm,file = 'C:/Users/sidharth.suman/Desktop/hacktemp/Stochastic_Gradient_Boosting.rda')
# summarize results
boosting_results <- resamples(list(c5.0=fit.c50, gbm=fit.gbm))
summary(boosting_results)
dotplot(boosting_results)

#Bagged CART
set.seed(seed)
fit.treebag <- train(default~., data=train, method="treebag", metric=metric, trControl=control)
predicted<- predict(fit.treebag,test[,-1])
measure_model_metrics(as.numeric(as.character(test$default)),as.numeric(as.character(predicted)))
save(fit.treebag,file = 'C:/Users/sidharth.suman/Desktop/hacktemp/treebag.rda')
text(fit.treebag$finalModel)
